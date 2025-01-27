﻿namespace Prelude.Data.Library.Endless

open System
open Percyqaz.Common
open Prelude
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Rulesets
open Prelude.Charts.Processing.Patterns
open Prelude.Data.Library
open Prelude.Data.Library.Collections
open Prelude.Data.User

type SuggestionContext =
    {
        BaseChart: ChartMeta * Rate
        Mods: ModState
        Filter: FilteredSearch
        MinimumRate: Rate
        MaximumRate: Rate
        OnlyNewCharts: bool
        RulesetId: string
        Ruleset: Ruleset
        Library: Library
        UserDatabase: UserDatabase
    }
    member this.LibraryViewContext: LibraryViewContext =
        {
            Rate = let (_, rate) = this.BaseChart in rate
            RulesetId = this.RulesetId
            Ruleset = this.Ruleset
            Library = this.Library
            UserDatabase = this.UserDatabase
        }

module Suggestion =

    let mutable recommended_already = Set.empty

    let most_common_pattern (total: Time) (patterns: PatternReport) =
        Array.tryHead patterns.Clusters
        |> Option.map _.Pattern
        |> Option.defaultValue Stream

    let private pattern_similarity (total: Time) (rate: Rate, patterns: PatternReport) (c_rate: Rate, c_patterns: PatternReport) : float32 =

        let c_total = c_patterns.Clusters |> Seq.sumBy _.Amount
        if most_common_pattern total patterns <> most_common_pattern c_total c_patterns then 0.0f
        else

        let mutable similarity = 0.0f
        for p2 in c_patterns.Clusters do
            for p1 in patterns.Clusters do
                if p1.Pattern = p2.Pattern then
                    let mixed_similarity = if p1.Mixed = p2.Mixed then 1.0f else 0.5f
                    let bpm_similarity =
                        let difference = (rate * float32 p1.BPM) / (c_rate * float32 p2.BPM) |> log |> abs
                        Math.Clamp(1.0f - 10.0f * difference, 0.0f, 1.0f)
                    let density_similarity =
                        let difference = (rate * p1.Density75) / (c_rate * p2.Density75) |> log |> abs
                        Math.Clamp(1.0f - 10.0f * difference, 0.0f, 1.0f)
                    similarity <- similarity + mixed_similarity * bpm_similarity * density_similarity * (p1.Amount / total) * (p2.Amount / c_total)
        similarity

    let get_random (filter_by: Filter) (ctx: LibraryViewContext) : ChartMeta option =
        let rand = Random()

        let charts =
            filter_by.Apply ctx.Library.Charts.Cache.Values
            |> Array.ofSeq

        if charts.Length > 0 then
            let result = charts.[rand.Next charts.Length]
            Some result
        else
            None

    let private get_core_suggestions (ctx: SuggestionContext) : (ChartMeta * Rate) seq =

        let base_chart, rate = ctx.BaseChart

        let patterns = base_chart.Patterns

        recommended_already <- Set.add base_chart.Hash recommended_already
        recommended_already <- Set.add (base_chart.Title.ToLower()) recommended_already

        let target_density = patterns.Density50 * rate

        let max_ln_pc = patterns.LNPercent + 0.1f
        let min_ln_pc = patterns.LNPercent - 0.1f

        let now = Timestamp.now ()
        let THIRTY_DAYS = 30L * 24L * 3600_000L

        let candidates =
            ctx.Library.Charts.Cache.Values
            |> Seq.filter (fun cc -> cc.Keys = base_chart.Keys)
            |> Seq.filter (fun cc -> not (recommended_already.Contains cc.Hash))
            |> Seq.filter (fun cc -> not (recommended_already.Contains (cc.Title.ToLower())))
            |> Seq.choose (fun cc ->
                let best_rate = target_density / cc.Patterns.Density50
                let best_approx_rate = round(best_rate / 0.05f<rate>) * 0.05f<rate>
                if best_approx_rate >= ctx.MinimumRate && best_approx_rate <= ctx.MaximumRate then
                    Some (cc, (best_approx_rate, cc.Patterns))
                else None
            )
            |> Seq.filter (fun (cc, (rate, p)) -> p.LNPercent >= min_ln_pc && p.LNPercent <= max_ln_pc)
            |> if ctx.OnlyNewCharts then
                Seq.filter (fun (cc, (rate, p)) -> now - (UserDatabase.get_chart_data cc.Hash ctx.UserDatabase).LastPlayed > THIRTY_DAYS)
               else
                id
            |> ctx.Filter.Apply

        let total_pattern_amount = patterns.Clusters |> Seq.sumBy _.Amount
        let spikiness = patterns.Density90 / patterns.Density50

        seq {
            for cc, (c_rate, c_patterns) in candidates do

                let sv_compatibility =
                    if (patterns.SVAmount < 30000.0f<ms>) <> (c_patterns.SVAmount < 30000.0f<ms>) then
                        0.5f
                    else 1.0f

                let length_compatibility =
                    let l1 = base_chart.Length / rate
                    let l2 = cc.Length / c_rate
                    1.0f - min 1.0f (abs (l2 - l1) / l1 * 10.0f)

                let difficulty_compatibility =
                    let c_spikiness = c_patterns.Density90 / c_patterns.Density50
                    1.0f - min 1.0f (abs (c_spikiness - spikiness) * 10.0f)

                let pattern_compatibility =
                    pattern_similarity total_pattern_amount (rate, patterns) (c_rate, c_patterns)

                let compatibility =
                    sv_compatibility * length_compatibility * difficulty_compatibility * pattern_compatibility

                yield (cc, c_rate), compatibility
        }
        |> Seq.sortByDescending snd
        |> Seq.map fst

    let get_suggestion (ctx: SuggestionContext) : (ChartMeta * Rate) option =
        let rand = Random()
        let best_matches = get_core_suggestions ctx |> Seq.truncate 50 |> Array.ofSeq

        if best_matches.Length = 0 then
            None
        else

            let cc, rate =
                let index =
                    rand.NextDouble()
                    |> fun x -> x * x
                    |> fun x -> x * float best_matches.Length
                    |> floor
                    |> int

                best_matches.[index]

            recommended_already <- Set.add cc.Hash recommended_already
            recommended_already <- Set.add (cc.Title.ToLower()) recommended_already

            Some (cc, rate)

type EndlessModeState =
    internal {
        mutable Playlist: string
        mutable Queue: (ChartMeta * (int * PlaylistEntryInfo)) list
    }

module EndlessModeState =

    let create () = { Playlist = ""; Queue = [] }

    let private shuffle_playlist_charts (items: 'T seq) =
        let random = new Random()
        items |> Seq.map (fun x -> x, random.Next()) |> Seq.sortBy snd |> Seq.map fst

    let queue_playlist (from: int) (name: string) (playlist: Playlist) (library: Library) (filter: FilteredSearch) (state: EndlessModeState) =
        state.Playlist <- name
        state.Queue <-
            playlist.Charts
            |> Seq.indexed
            |> Seq.skip from
            |> Seq.choose (fun (i, (c, info)) ->
                match ChartDatabase.get_meta c.Hash library.Charts with
                | Some cc -> Some(cc, (i, info))
                | None -> None
            )
            |> filter.Apply
            |> List.ofSeq

    let queue_shuffled_playlist (name: string) (playlist: Playlist) (library: Library) (filter: FilteredSearch) (state: EndlessModeState) =
        state.Playlist <- name
        state.Queue <-
            playlist.Charts
            |> Seq.indexed
            |> Seq.choose (fun (i, (c, info)) ->
                match ChartDatabase.get_meta c.Hash library.Charts with
                | Some cc -> Some(cc, (i, info))
                | None -> None
            )
            |> filter.Apply
            |> shuffle_playlist_charts
            |> List.ofSeq

    let clear_queue (state: EndlessModeState) =
        state.Playlist <- ""
        state.Queue <- []

    type Next =
        {
            Chart: ChartMeta
            Rate: Rate
            Mods: ModState
            LibraryContext: LibraryContext
            NextContext: SuggestionContext
        }

    let next (ctx: SuggestionContext) (state: EndlessModeState) : Next option =
        match state.Queue with
        | (chart, (index, playlist_data)) :: xs ->
            state.Queue <- xs
            Some
                {
                    Chart = chart
                    Rate = playlist_data.Rate.Value
                    Mods = playlist_data.Mods.Value
                    LibraryContext = LibraryContext.Playlist(index, state.Playlist, playlist_data)
                    NextContext = ctx
                }
        | [] ->
            match Suggestion.get_suggestion ctx with
            | Some (next_cc, rate) ->
                Some
                    {
                        Chart = next_cc
                        Rate = rate
                        Mods = ctx.Mods
                        LibraryContext = LibraryContext.None
                        NextContext = ctx
                    }
            | None -> None