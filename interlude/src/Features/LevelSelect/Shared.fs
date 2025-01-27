﻿namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Charts.Processing
open Prelude.Data.User
open Prelude.Data.Library
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Interlude.UI
open Interlude.Content
open Interlude.Options
open Interlude.Features.Pacemaker
open Interlude.Features.Play
open Interlude.Features.Play.Replay
open Interlude.Features.Online
open Interlude.Features.Collections
open Interlude.Features.Gameplay

module LevelSelect =

    let private refresh_all_event = Event<unit>()
    let private refresh_details_event = Event<unit>()

    let refresh_all () = GameThread.defer refresh_all_event.Trigger
    let refresh_details () = GameThread.defer refresh_details_event.Trigger

    let on_refresh_all = refresh_all_event.Publish
    let on_refresh_details = refresh_details_event.Publish

    do
        Content.OnChartAdded.Add (fun () -> if Screen.current_type = Screen.Type.LevelSelect then refresh_all())

        CollectionActions.collection_modified.Add(fun () ->
            if options.ChartGroupMode.Value = "collection" || (options.ChartGroupMode.Value <> "level" && options.TreeAlwaysShowCollections.Value) then
                refresh_all ()
        )
        CollectionActions.likes_modified.Add(fun () ->
            match SelectedChart.LIBRARY_CTX with
            | LibraryContext.Likes -> refresh_all()
            | _ -> refresh_details()
        )

    let mutable filter: FilteredSearch = FilteredSearch.Empty

    module History =

        let mutable private history: (ChartMeta * Rate) list = []
        let mutable private forward_history: (ChartMeta * Rate) list = []

        let append_current () =
            match SelectedChart.CACHE_DATA with
            | Some cc ->
                forward_history <- []
                history <- (cc, SelectedChart.rate.Value) :: history
            | None -> ()

        let back () =
            if not (Transitions.in_progress()) then
                match history with
                | (cc, rate) :: xs ->

                    match SelectedChart.CACHE_DATA with
                    | Some cc -> forward_history <- (cc, SelectedChart.rate.Value) :: forward_history
                    | None -> ()

                    history <- xs
                    SelectedChart._rate.Set rate
                    SelectedChart.change(cc, LibraryContext.None, true)
                | _ -> ()

        let can_go_back() = (List.isEmpty >> not) history

        let forward () =
            if not (Transitions.in_progress()) then
                match forward_history with
                | (cc, rate) :: xs ->

                    match SelectedChart.CACHE_DATA with
                    | Some cc -> history <- (cc, SelectedChart.rate.Value) :: history
                    | None -> ()

                    forward_history <- xs
                    SelectedChart._rate.Set rate
                    SelectedChart.change(cc, LibraryContext.None, true)
                | _ -> ()

        let can_go_forward() = (List.isEmpty >> not) forward_history

    let private state: EndlessModeState = EndlessModeState.create()
    let mutable private suggestion_ctx : SuggestionContext option = None

    let private enter_gameplay (ctx: LibraryContext) =
        match ctx with
        | LibraryContext.Playlist(index, playlist_id, data) ->
            match Content.Library.Collections.GetPlaylist playlist_id with
            | Some playlist -> EndlessModeState.queue_playlist (index + 1) playlist_id playlist Content.Library filter state
            | None -> ()
        | _ -> ()

    let play (info: LoadedChartInfo) =
        if
            Screen.change_new
                (fun () ->
                    PlayScreen.play_screen (
                        info,
                        if options.EnablePacemaker.Value then
                            PacemakerCreationContext.FromUserSetting
                        else
                            PacemakerCreationContext.None
                    )
                )
                Screen.Type.Play
                Transitions.EnterGameplayFadeAudio
        then enter_gameplay info.LibraryContext

    let continue_endless_mode () : bool =
        if Transitions.in_progress() then false else

        let ctx =
            match suggestion_ctx with
            | None ->
                {
                    BaseChart = SelectedChart.CACHE_DATA.Value, SelectedChart.rate.Value
                    MinimumRate = if options.SuggestionsEnableRates.Value then options.SuggestionsMinRate.Value else SelectedChart.rate.Value
                    MaximumRate = if options.SuggestionsEnableRates.Value then options.SuggestionsMaxRate.Value else SelectedChart.rate.Value
                    OnlyNewCharts = options.SuggestionsOnlyNew.Value
                    Filter = filter.WithoutSearchTerms
                    Mods = SelectedChart.selected_mods.Value
                    RulesetId = Rulesets.current_hash
                    Ruleset = Rulesets.current
                    Library = Content.Library
                    UserDatabase = Content.UserData
                }
            | Some ctx -> ctx

        match EndlessModeState.next ctx state with
        | Some next ->
            History.append_current()
            SelectedChart._rate.Set next.Rate
            SelectedChart._selected_mods.Set next.Mods
            SelectedChart.change (next.Chart, next.LibraryContext, false)
            SelectedChart.when_loaded true (fun info -> play info)
            suggestion_ctx <- Some next.NextContext
            true
        | None ->
            Notifications.action_feedback (Icons.ALERT_CIRCLE, %"notification.suggestion_failed", "")
            false

    let start_playlist (playlist_id: string, playlist: Playlist) =
        EndlessModeState.queue_playlist 0 playlist_id playlist Content.Library filter state
        continue_endless_mode() |> ignore

    let start_playlist_shuffled (playlist_id: string, playlist: Playlist) =
        EndlessModeState.queue_shuffled_playlist playlist_id playlist Content.Library filter state
        continue_endless_mode() |> ignore

    let exit_gameplay () =
        suggestion_ctx <- None
        EndlessModeState.clear_queue state

    let random_chart () =
        let true_random_chart() =
            let ctx =
                {
                    Rate = SelectedChart.rate.Value
                    RulesetId = Rulesets.current_hash
                    Ruleset = Rulesets.current
                    Library = Content.Library
                    UserDatabase = Content.UserData
                }

            match Suggestion.get_random filter.Filter ctx with
            | Some cc ->
                History.append_current()
                SelectedChart.change(cc, LibraryContext.None, true)
            | None -> ()

        if not (Transitions.in_progress()) then
            if SelectedChart.RATING.IsSome then
                let ctx =
                    {
                        BaseChart = SelectedChart.CACHE_DATA.Value, SelectedChart.rate.Value
                        MinimumRate = if options.SuggestionsEnableRates.Value then options.SuggestionsMinRate.Value else SelectedChart.rate.Value
                        MaximumRate = if options.SuggestionsEnableRates.Value then options.SuggestionsMaxRate.Value else SelectedChart.rate.Value
                        OnlyNewCharts = false
                        Filter = filter
                        Mods = SelectedChart.selected_mods.Value
                        RulesetId = Rulesets.current_hash
                        Ruleset = Rulesets.current
                        Library = Content.Library
                        UserDatabase = Content.UserData
                    }

                match Suggestion.get_suggestion ctx with
                | Some (cc, rate) ->
                    History.append_current()
                    SelectedChart._rate.Value <- rate
                    SelectedChart.change(cc, LibraryContext.None, true)
                | None ->
                    true_random_chart()
            else
                true_random_chart()

    let choose_this_chart () =
        SelectedChart.when_loaded true
        <| fun info ->

            match Network.lobby with
            | Some lobby ->
                if Screen.change Screen.Type.Lobby Transitions.Default then
                    lobby.SelectChart (info.ChartMeta, SelectedChart.rate.Value, SelectedChart.selected_mods.Value)
            | None ->
                if SelectedChart.autoplay then
                    Screen.change_new
                        (fun () -> ReplayScreen.replay_screen (info.Chart, ReplayMode.Auto info.WithColors) :> Screen.T)
                        Screen.Type.Replay
                        Transitions.Default
                    |> ignore
                else
                    play info

    let challenge_score (score_info: ScoreInfo) =
        SelectedChart.if_loaded
        <| fun info ->

            if
                Screen.change_new
                    (fun () -> PlayScreen.play_screen (info, PacemakerCreationContext.FromScore score_info))
                    Screen.Type.Play
                    Transitions.EnterGameplayFadeAudio
            then
                enter_gameplay info.LibraryContext
                SelectedChart.rate.Set score_info.Rate
                SelectedChart.selected_mods.Set score_info.Mods

    let watch_replay (score_info: ScoreInfo, with_colors: ColoredChart) =
        if
            Screen.change_new
                (fun () ->
                    ReplayScreen.replay_screen (score_info.Chart, ReplayMode.Replay(score_info, with_colors))
                    :> Screen.T
                )
                Screen.Type.Replay
                Transitions.Default
        then
            SelectedChart.rate.Value <- score_info.Rate

// todo: rename; relocate
[<RequireQualifiedAccess>]
type Display =
    | Local
    | Online
    | Patterns