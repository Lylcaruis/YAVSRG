﻿namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Gameplay
open Prelude.Data.User
open Interlude.UI

type SkillTimeline() =
    inherit Container(NodeType.None)

    let graph_container = SwapContainer(Position = Position.ShrinkT(50.0f))

    let day_range = Animation.Fade(90.0f)
    let day_offset = Animation.Fade(30.0f)
    let keymode = Setting.simple 4

    let refresh_graph() =
        graph_container.Current <- SkillTimelineGraph(keymode.Value, day_range, day_offset)

    override this.Init(parent) =
        let available_keymodes =
            seq {
                for i = 3 to 10 do
                    if Stats.TOTAL_STATS.KeymodeSkills.[i - 3] <> KeymodeSkillBreakdown.Default then
                        yield i
            }
            |> Array.ofSeq

        let available_keymodes = if available_keymodes.Length = 0 then [|4|] else available_keymodes

        keymode.Value <- available_keymodes.[0]
        refresh_graph()

        let keymode_switcher =
            StylishButton(
                (fun () ->
                    keymode.Value <- available_keymodes.[(1 + Array.findIndex ((=) keymode.Value) available_keymodes) % available_keymodes.Length]
                    refresh_graph()
                ),
                (fun () -> sprintf "%iK" keymode.Value),
                K Colors.shadow_2.O2,
                TiltRight = false,
                Position = Position.SliceT(50.0f).SliceR(150.0f)
            )

        let zoom_in =
            StylishButton(
                (fun () -> day_range.Target <- max 30.0f (day_range.Target - 30.0f)),
                K Icons.ZOOM_IN,
                K Colors.black.O2,
                Hotkey = "uprate",
                Position = Position.SliceT(50.0f).ShrinkR(175.0f).SliceR(100.0f)
            )

        let zoom_out =
            StylishButton(
                (fun () -> day_range.Target <- min 390.0f (day_range.Target + 30.0f)),
                K Icons.ZOOM_OUT,
                K Colors.shadow_2.O2,
                Hotkey = "downrate",
                Position = Position.SliceT(50.0f).ShrinkR(300.0f).SliceR(100.0f)
            )

        let show_newer =
            StylishButton(
                (fun () -> day_offset.Target <- max 0.0f (day_offset.Target - day_range.Target * 0.25f)),
                K Icons.ARROW_RIGHT,
                K Colors.black.O2,
                Position = Position.SliceT(50.0f).ShrinkR(425.0f).SliceR(100.0f)
            )

        let show_older =
            StylishButton(
                (fun () -> day_offset.Target <- day_offset.Target + day_range.Target * 0.25f),
                K Icons.ARROW_LEFT,
                K Colors.shadow_2.O2,
                Position = Position.SliceT(50.0f).ShrinkR(550.0f).SliceR(100.0f)
            )

        this
        |+ keymode_switcher
        |+ zoom_in
        |+ zoom_out
        |+ show_newer
        |+ show_older
        |* graph_container
        base.Init parent

    member this.Switch(k: int) =
        keymode.Value <- k
        refresh_graph()