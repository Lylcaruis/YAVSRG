namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring
open Interlude.UI
open Interlude.Content
open Prelude.Gameplay.Rulesets

type RulesetWindowGraph(ruleset: Setting<Ruleset>) =
    inherit StaticWidget(NodeType.None)
    let accuracy_points =
        match ruleset.Value.Accuracy with
        | AccuracyPoints.WifeCurve _ -> failwith "crash if points don't exist for this ruleset"
        | AccuracyPoints.PointsPerJudgement points -> points
    override this.Draw() =
        Render.rect (
            Rect.FromEdges(this.Bounds.Left, this.Bounds.Top, this.Bounds.Right, this.Bounds.Bottom)
        ) Colors.black.O2
        Render.border Style.PADDING this.Bounds Colors.white
        let judgement =
            fun p1 p2 p3 p4->
                let y = this.Bounds.Height / -p4 * (-p4 - 1f)
                let biggestwindow = p3
                let halfsize = 5f
                let xcalc = this.Bounds.CenterX + p2 * (this.Bounds.Width - halfsize * 2f) / biggestwindow / 2f
                let ycalc =
                    let graphHeight = this.Bounds.Height - halfsize * 2f
                    let normalized = (p1 - p4) / (1f - p4)
                    this.Bounds.Top + halfsize + (1f - normalized) * graphHeight
                Rect.FromEdges(
                        System.Math.Clamp(xcalc - halfsize,
                        this.Bounds.Left,
                        this.Bounds.Right),               
                        System.Math.Clamp(ycalc + halfsize,
                        this.Bounds.Top,
                        this.Bounds.Bottom),
                        System.Math.Clamp(xcalc + halfsize,
                        this.Bounds.Left,
                        this.Bounds.Right),              
                        System.Math.Clamp(ycalc - halfsize,
                        this.Bounds.Top,
                        this.Bounds.Bottom))
        let linecentre =
            fun p1 p2 p3 p4->
                let y = this.Bounds.Height / -p4 * (-p4 - 1f)
                let biggestwindow = p3
                let halfsize = 5f
                let xcalc = this.Bounds.CenterX + (p2 / 2f) * (this.Bounds.Width - halfsize * 2f) / biggestwindow / 2f
                let ycalc =
                    let graphHeight = this.Bounds.Height - halfsize * 2f
                    let normalized = (p1 - p4) / (1f - p4)
                    this.Bounds.Top + halfsize + (1f - normalized) * graphHeight
                Rect.FromEdges(
                        System.Math.Clamp(xcalc - halfsize,
                        this.Bounds.Left,
                        this.Bounds.Right),               
                        System.Math.Clamp(ycalc + halfsize,
                        this.Bounds.Top,
                        this.Bounds.Bottom),
                        System.Math.Clamp(xcalc + halfsize,
                        this.Bounds.Left,
                        this.Bounds.Right),              
                        System.Math.Clamp(ycalc - halfsize,
                        this.Bounds.Top,
                        this.Bounds.Bottom))
        let box =
            fun p1 p2 p3 p4->
                let y = this.Bounds.Height / -p4 * (-p4 - 1f)
                let biggestwindow = p3
                let halfsize = 5f
                let xcalc = this.Bounds.CenterX + p2 * (this.Bounds.Width - halfsize * 2f) / biggestwindow / 2f               
                let graphHeight = this.Bounds.Height - halfsize * 2f
                let normalized = (p1 - p4) / (1f - p4)
                let normalizedzero = (0f - p4) / (1f - p4)
                let zeroy = this.Bounds.Top + halfsize + (1f - normalizedzero) * graphHeight
                let ycalc = this.Bounds.Top + halfsize + (1f - normalized) * graphHeight
                Rect.FromEdges(
                        System.Math.Clamp(xcalc,
                        this.Bounds.Left,
                        this.Bounds.Right),               
                        System.Math.Clamp(ycalc,
                        this.Bounds.Top,
                        this.Bounds.Bottom),
                        System.Math.Clamp(this.Bounds.CenterX,
                        this.Bounds.Left,
                        this.Bounds.Right),              
                        System.Math.Clamp(this.Bounds.Bottom,
                        this.Bounds.Top,
                        this.Bounds.Bottom))
        for i = 0 to ruleset.Value.Judgements.Length - 1 do

            let j = ruleset.Value.Judgements.[i]
            let all_others = 
                ruleset.Value.Judgements
                |> Array.mapi (fun idx j -> idx, j)
                |> Array.filter (fun (idx, _) -> idx <> i)

            let largest_window =
                all_others
                |> Array.choose (fun (_, j) ->
                    match j.TimingWindows with
                    | Some (early, late) -> Some [| abs early; abs late |]
                    | None -> None)
                |> Array.collect id
                |> Array.max
            let lowest_accuracy =
                match ruleset.Value.Accuracy with
                | AccuracyPoints.PointsPerJudgement points -> Array.min points


            match j.TimingWindows with
                | Some (early, late) -> 
                    let rect = judgement (float32 accuracy_points.[i]) (float32 early) (float32 largest_window) (float32 lowest_accuracy)
                    Render.rect
                        rect
                        j.Color
                    let rect = box (float32 accuracy_points.[i]) (float32 early) (float32 largest_window) (float32 lowest_accuracy)
                    Render.rect
                        rect
                        j.Color.O1
                    //let rect = linecentre (float32 accuracy_points.[i]) (float32 early) (float32 largest_window) (float32 lowest_accuracy)
                    //Render.rect
                    //    rect
                    //    j.Color
                    let rect = judgement (float32 accuracy_points.[i]) (float32 late) (float32 largest_window) (float32 lowest_accuracy)
                    Render.rect
                        rect
                        j.Color
                    let rect = box (float32 accuracy_points.[i]) (float32 late) (float32 largest_window) (float32 lowest_accuracy)
                    Render.rect
                        rect
                        j.Color.O1
                    //let rect = linecentre (float32 accuracy_points.[i]) (float32 early) (float32 largest_window) (float32 lowest_accuracy)
                    //Render.rect
                    //    rect
                    //    j.Color
                | None -> 
                    let rect = judgement (float32 accuracy_points.[i]) (float32 largest_window) (float32 largest_window) (float32 lowest_accuracy)
                    Render.rect
                        rect
                        j.Color

    override this.Update(elapsed_ms, moved) =
        // updates that happen each frame and don't draw anything go here
        // if there is nothing this method is not necessary
        base.Update(elapsed_ms, moved)

type ConfigureAccuracyPage(ruleset: Setting<Ruleset>) =
    inherit Page()

    let is_wife_curve =
        match ruleset.Value.Accuracy with
        | AccuracyPoints.WifeCurve _ -> true
        | AccuracyPoints.PointsPerJudgement _ -> false
        |> Setting.simple

    let wife_judge =
        match ruleset.Value.Accuracy with
        | AccuracyPoints.WifeCurve j -> j
        | AccuracyPoints.PointsPerJudgement _ -> 4
        |> Setting.simple

    let points_per_judgement : float array =
        match ruleset.Value.Accuracy with
        | AccuracyPoints.WifeCurve _ -> Array.create ruleset.Value.Judgements.Length 1.0
        | AccuracyPoints.PointsPerJudgement p -> p

    let decimal_places = Setting.simple ruleset.Value.Formatting.DecimalPlaces

    member this.SaveChanges() =
        ruleset.Set
            { ruleset.Value with
                Formatting = { DecimalPlaces = decimal_places.Value }
                Accuracy =
                    if is_wife_curve.Value then
                        AccuracyPoints.WifeCurve wife_judge.Value
                    else
                        AccuracyPoints.PointsPerJudgement points_per_judgement
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)

        let judgements_container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

        for i, j in ruleset.Value.Judgements |> Array.indexed do
            let setting =
                Setting.make
                    (fun v -> points_per_judgement.[i] <- v)
                    (fun () -> points_per_judgement.[i])
                |> Setting.bound (-10.0, 1.0)
            judgements_container.Add (PageSetting(j.Name, NumberEntry.Create setting))

        page_container()
            .With(
                PageSetting(%"rulesets.accuracy.decimal_places",
                    SelectDropdown([| DecimalPlaces.TWO, "2"; DecimalPlaces.THREE, "3"; DecimalPlaces.FOUR, "4" |], decimal_places)
                )
                    .Pos(0),
                PageSetting(%"rulesets.accuracy.accuracy_type",
                    SelectDropdown([| true, "Wife3"; false, %"rulesets.accuracy.accuracy_type.points_per_judgement" |], is_wife_curve)
                )
                    .Pos(3),
                PageSetting(%"rulesets.accuracy.wife_judge",
                    SelectDropdown([| 4, "4"; 5, "5"; 6, "6"; 7, "7"; 8, "8"; 9, "JUSTICE"|], wife_judge)
                )
                    .Conditional(is_wife_curve.Get)
                    .Pos(5),
                ScrollContainer(judgements_container)
                    .Conditional(is_wife_curve.Get >> not)
                    .Pos(5, PAGE_BOTTOM - 5)
            )
    override this.Title = %"rulesets.edit.accuracy"
    override this.Init(parent: Widget) =
        base.Init parent
        this
            .Add(
                RulesetWindowGraph(ruleset: Setting<Ruleset>)
                    .Position(Position.SlicePercentL(0.98f).SlicePercentT(0.98f).SlicePercentR(0.4f).SlicePercentB(0.5f))
                )
