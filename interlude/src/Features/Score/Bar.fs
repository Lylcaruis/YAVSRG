namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.Options
open Interlude.UI
open Interlude.Features.Score
open System.Collections.Generic

type StreakSegment =
    {
        StartTime: float32
        EndTime: float32
        StreakRank: int
    }
module BarSettings =

    let column_filter = Array.create 10 true

    let COLUMN_FILTER_KEYS =
        [|
            Keys.D1
            Keys.D2
            Keys.D3
            Keys.D4
            Keys.D5
            Keys.D6
            Keys.D7
            Keys.D8
            Keys.D9
            Keys.D0
        |]
        |> Array.map Bind.mk

type ScoreBarSettingsPage(keys: int, apply_column_filter: unit -> unit) =
    inherit Page()
    override this.Content() =
        page_container()
            .With(
            )
    override this.Title = "Score Bar Settings"  // <- You must provide a string here
and ScoreBar(score_info: ScoreInfo, stats: ScoreScreenStats ref) =
    inherit StaticWidget(NodeType.None)
    let fbo = Render.borrow_fbo()

    let NORMAL_POSITION =
        {
            Left = 0.35f %+ 30.0f
            Top = 0.0f %- 100.0f
            Right = 1.0f %- 20.0f
            Bottom = 0.0f %- 5.0f
        }

    let EXPANDED_POSITION =
        {
            Left = 0.0f %+ 20.0f
            Top = -(1.0f / 0.35f - 1.0f) %+ 390.0f
            Right = 1.0f %- 20.0f
            Bottom = 1.0f %- 65.0f
        }

    do fbo.Unbind()
    override this.Init(parent) =
        this.Position <- NORMAL_POSITION
        base.Init parent

    member private this.Drawbars() =  
        let events = score_info.Scoring.Events |> Seq.toList
        let threshold = 10 // Optional: you can raise this if you want to ignore ultra-short streaks
        let getJudgementColor (rank: int) =
            if rank >= 0 && rank < score_info.Ruleset.Judgements.Length then
                let judgement = score_info.Ruleset.Judgements.[rank]
                judgement.Color
            else
                Color.Gray // fallback

        let segments = ResizeArray<StreakSegment>()
        let mutable currentLength = 0
        let mutable streakStartTime = 0.0f
        let mutable streakRank: int option = None

        for i in 0 .. events.Length - 1 do
            let event = events.[i]
            match event.Action.Judgement with
            | Some (rank, _) ->
                let time = float32 event.Time
                match streakRank with
                | None ->
                    // First judgement — start a new streak
                    streakRank <- Some rank
                    streakStartTime <- time
                    currentLength <- 1
                | Some r ->
                    if rank <= r then
                        // Continue — still hitting that rank or better
                        currentLength <- currentLength + 1
                    else
                        // Worse — flush current streak and start new one
                        if currentLength >= threshold then
                            segments.Add({
                                StartTime = streakStartTime
                                EndTime = time
                                StreakRank = r // use original streak rank
                            })
                        streakRank <- Some rank
                        streakStartTime <- time
                        currentLength <- 1

            | None ->
                // Miss or no judgement — flush current streak
                let time = float32 event.Time
                match streakRank with
                | Some r when currentLength >= threshold ->
                    segments.Add({
                        StartTime = streakStartTime
                        EndTime = time
                        StreakRank = r
                    })
                | _ -> ()
                streakRank <- None
                currentLength <- 0

        // Final flush
        match streakRank with
        | Some r when currentLength >= threshold ->
            let endTime = float32 (events.[events.Length - 1].Time)
            segments.Add({
                StartTime = streakStartTime
                EndTime = endTime
                StreakRank = r
            })
        | _ -> ()

        // Rendering
        let graphBounds = this.Bounds
        let timeRangeStart = 0.0f
        let timeRangeEnd = float32 (events.[events.Length - 1].Time / score_info.Rate)

        let chartTimeToScreenX (time: float32) (graphBounds: Rect) (timeStart: float32) (timeEnd: float32) =
            let tNorm = (time - timeStart) / (timeEnd - timeStart)
            graphBounds.Left + tNorm * graphBounds.Width

        for seg in segments do
            let x1 = chartTimeToScreenX seg.StartTime graphBounds timeRangeStart timeRangeEnd
            let x2 = chartTimeToScreenX seg.EndTime graphBounds timeRangeStart timeRangeEnd
            let color = getJudgementColor seg.StreakRank
            Render.rect_edges x1 this.Bounds.Top x2 this.Bounds.Bottom color
    member private this.Redraw() =
        ()
    override this.Draw() =
        this.Drawbars()
        
    interface System.IDisposable with
        override this.Dispose() =
            for i = 0 to 9 do GraphSettings.column_filter.[i] <- true
            (fbo :> System.IDisposable).Dispose()
           