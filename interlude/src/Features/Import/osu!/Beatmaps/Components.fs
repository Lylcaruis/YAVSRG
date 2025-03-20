﻿namespace Interlude.Features.Import.osu

open System
open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Imports
open Interlude.Content
open Interlude.UI

type private BeatmapDownloadStatus =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed

type private BeatmapImportCard(data: MinoBeatmapSet) as this =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                this.Download()
            )
        )

    let mutable status = NotDownloaded
    let mutable progress = 0.0f

    let download () =
        if status = NotDownloaded || status = DownloadFailed then

            let task = OnlineImports.download_osu_set(sprintf "https://catboy.best/d/%in" data.id, Content.Charts, Content.UserData, ImportProgress.log_progress_bar data.title)
            import_queue.Request(task,
                function
                | Ok result ->
                    Notifications.task_feedback (
                        Icons.DOWNLOAD,
                        %"notification.install_song",
                        [data.title; result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString()] %> "notification.install_song.body"
                    )
                    Content.TriggerChartAdded()
                    progress <- 1.0f
                    status <- Installed
                | Error reason ->
                    Logging.Error "Error importing %s: %s" data.title reason
                    Notifications.error (%"notification.install_song_failed", data.title)
                    status <- DownloadFailed
            )

            status <- Downloading

    let fill, border, ranked_status =
        match data.status with
        | "ranked" -> Colors.cyan, Colors.cyan_accent, "Ranked"
        | "qualified" -> Colors.green, Colors.green_accent, "Qualified"
        | "loved" -> Colors.pink, Colors.pink_accent, "Loved"
        | "pending" -> Colors.grey_2, Colors.grey_1, "Pending"
        | "wip" -> Colors.grey_2, Colors.grey_1, "WIP"
        | "graveyard"
        | _ -> Colors.grey_2, Colors.grey_1, "Graveyard"

    let beatmaps = data.beatmaps |> Array.filter (fun x -> x.mode = "mania")

    let keymodes_string =
        let modes =
            beatmaps
            |> Seq.map (fun bm -> int bm.cs)
            |> Seq.distinct
            |> Seq.sort
            |> Array.ofSeq

        if modes.Length > 3 then
            sprintf "%i-%iK" modes.[0] modes.[modes.Length - 1]
        else
            modes |> Seq.map (fun k -> sprintf "%iK" k) |> String.concat ", "

    override this.Init(parent) =
        this
        |+ Frame(
            Fill = (fun () -> if this.Focused then fill.O3 else fill.O2),
            Border = fun () -> if this.Focused then Colors.white else border.O2
        )
        //|+ Button(Icons.OPEN_IN_BROWSER,
        //    fun () -> openUrl(sprintf "https://osu.ppy.sh/beatmapsets/%i" data.beatmapset_id)
        //    ,
        //    Position = Position.SliceRight(160.0f).TrimRight(80.0f).Margin(5.0f, 10.0f))
        |* Clickable.Focus this
        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        base.Draw()

        match status with
        | Downloading -> Render.rect (this.Bounds.SliceL(this.Bounds.Width * progress)) Colors.white.O1
        | _ -> ()

        Text.fill_b (
            Style.font,
            data.title,
            this.Bounds.SliceT(45.0f).Shrink(10.0f, 0.0f),
            Colors.text,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            data.artist + "  •  " + data.creator,
            this.Bounds.SliceB(45.0f).Shrink(10.0f, 5.0f),
            Colors.text_subheading,
            Alignment.LEFT
        )

        let status_bounds =
            this.Bounds.SliceB(40.0f).SliceR(150.0f).Shrink(5.0f, 0.0f)

        Render.rect status_bounds Colors.shadow_2.O2

        Text.fill_b (
            Style.font,
            ranked_status,
            status_bounds.Shrink(5.0f, 0.0f).ShrinkB(5.0f),
            (border, Colors.shadow_2),
            Alignment.CENTER
        )

        let download_bounds =
            this.Bounds.SliceT(40.0f).SliceR(300.0f).Shrink(5.0f, 0.0f)

        Render.rect download_bounds Colors.shadow_2.O2

        Text.fill_b (
            Style.font,
            (match status with
             | NotDownloaded -> Icons.DOWNLOAD + " Download"
             | Downloading -> Icons.DOWNLOAD + " Downloading .."
             | DownloadFailed -> Icons.X + " Error"
             | Installed -> Icons.CHECK + " Downloaded"),
            download_bounds.Shrink(5.0f, 0.0f).ShrinkB(5.0f),
            (match status with
             | NotDownloaded -> if this.Focused then Colors.text_yellow_2 else Colors.text
             | Downloading -> Colors.text_yellow_2
             | DownloadFailed -> Colors.text_red
             | Installed -> Colors.text_green),
            Alignment.CENTER
        )

        let stat x text =
            let stat_bounds = this.Bounds.SliceB(40.0f).ShrinkR(x).SliceR(145.0f)
            Render.rect stat_bounds Colors.shadow_2.O2

            Text.fill_b (
                Style.font,
                text,
                stat_bounds.Shrink(5.0f, 0.0f).ShrinkB(5.0f),
                Colors.text_subheading,
                Alignment.CENTER
            )

        stat 150.0f (sprintf "%s %i" Icons.HEART data.favourite_count)
        stat 300.0f (sprintf "%s %i" Icons.PLAY data.play_count)
        stat 450.0f keymodes_string

        if this.Focused && Mouse.x () > this.Bounds.Right - 600.0f then
            let popover_bounds =
                Rect.Box(
                    this.Bounds.Right - 900.0f,
                    this.Bounds.Bottom + 10.0f,
                    600.0f,
                    45.0f * float32 beatmaps.Length
                )

            Render.rect popover_bounds Colors.shadow_2.O3
            let mutable y = 0.0f

            for beatmap in beatmaps do
                Text.fill_b (
                    Style.font,
                    beatmap.version,
                    popover_bounds.SliceT(45.0f).Translate(0.0f, y).Shrink(10.0f, 5.0f),
                    Colors.text,
                    Alignment.LEFT
                )

                Text.fill_b (
                    Style.font,
                    sprintf "%.2f*" beatmap.difficulty_rating,
                    popover_bounds.SliceT(45.0f).Translate(0.0f, y).Shrink(10.0f, 5.0f),
                    Colors.text,
                    Alignment.RIGHT
                )

                y <- y + 45.0f

    member private this.Download() = download ()

type private SortingDropdown
    (options: (string * string) seq, label: string, setting: Setting<string>, reverse: Setting<bool>, bind: Hotkey) =
    inherit Container(NodeType.None)

    let mutable display_value =
        Seq.find (fun (id, _) -> id = setting.Value) options |> snd

    let dropdown_wrapper = DropdownWrapper(fun d -> Position.SliceT(d.Height + 60.0f).ShrinkT(60.0f).Shrink(Style.PADDING, 0.0f))

    override this.Init(parent: Widget) =
        this
        |+ StylishButton(
            (fun () -> this.ToggleDropdown()),
            K(label + ":"),
            !%Palette.HIGHLIGHT_100,
            Hotkey = bind,
            Position = Position.SliceL 120.0f
        )
        |+ StylishButton(
            (fun () -> reverse.Value <- not reverse.Value),
            (fun () ->
                sprintf
                    "%s %s"
                    display_value
                    (if reverse.Value then
                         Icons.CHEVRONS_DOWN
                     else
                         Icons.CHEVRONS_UP)
            ),
            !%Palette.DARK_100,
            TiltRight = false,
            Position = Position.ShrinkL 145.0f
        )
        |* dropdown_wrapper

        base.Init parent

    member this.ToggleDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            Dropdown
                {
                    Items = options
                    ColorFunc = K Colors.text
                    Setting =
                        setting
                        |> Setting.trigger (fun v ->
                            display_value <- Seq.find (fun (id, _) -> id = v) options |> snd
                        )
                }
        )