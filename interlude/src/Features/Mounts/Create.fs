﻿namespace Interlude.Features.Mounts

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts.Conversions
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI
open Interlude.Features.Import

type private CreateMountPage(game: MountedGameType, setting: Setting<Imports.MountedChartSource option>) =
    inherit Page()

    let auto_detect_location =
        match game with
        | MountedGameType.Osu -> Imports.OSU_SONG_FOLDER
        | MountedGameType.Quaver -> Imports.QUAVER_SONG_FOLDER
        | MountedGameType.Stepmania -> Imports.STEPMANIA_PACK_FOLDER
        | MountedGameType.Etterna -> Imports.ETTERNA_PACK_FOLDER

    let folder_detected = System.IO.Directory.Exists auto_detect_location

    let info =
        match game with
            | MountedGameType.Osu ->
                Callout.Normal
                    .Icon(Icons.DOWNLOAD)
                    .Title(%"mount.create.osu.prompt")
                    .Body(%"mount.create.folder_hint")
            | MountedGameType.Quaver ->
                Callout.Normal
                    .Icon(Icons.DOWNLOAD)
                    .Title(%"mount.create.quaver.prompt")
                    .Body(%"mount.create.folder_hint")
            | MountedGameType.Stepmania ->
                Callout.Normal
                    .Icon(Icons.DOWNLOAD)
                    .Title(%"mount.create.stepmania.prompt")
                    .Body(%"mount.create.folder_hint")
            | MountedGameType.Etterna ->
                Callout.Normal
                    .Icon(Icons.DOWNLOAD)
                    .Title(%"mount.create.etterna.prompt")
                    .Body(%"mount.create.folder_hint")

    override this.Content() =
        FileDrop.on_file_drop <-
            fun path ->
                match game, path with
                | MountedGameType.Osu, PackFolder -> setting.Value <- Imports.MountedChartSource.Pack("osu!", path) |> Some
                | MountedGameType.Osu, _ -> Notifications.error (%"mount.create.osu.error", "")
                | MountedGameType.Quaver, PackFolder -> setting.Value <- Imports.MountedChartSource.Pack("Quaver", path) |> Some
                | MountedGameType.Quaver, _ -> Notifications.error (%"mount.create.quaver.error", "")
                | MountedGameType.Stepmania, FolderOfPacks
                | MountedGameType.Etterna, FolderOfPacks -> setting.Value <- Imports.MountedChartSource.Library path |> Some
                | MountedGameType.Stepmania, _ -> Notifications.error (%"mount.create.stepmania.error", "")
                | MountedGameType.Etterna, _ -> Notifications.error (%"mount.create.etterna.error", "")

                if setting.Value.IsSome then
                    Imports.import_mounted_source.Request(
                        (setting.Value.Value, Content.Library),
                        fun result ->
                            Notifications.task_feedback (
                                Icons.CHECK,
                                %"notification.import_success",
                                [result.ConvertedCharts.ToString(); result.SkippedCharts.Length.ToString()] %> "notification.import_success.body"
                            )
                            Content.TriggerChartAdded()
                    )
                    Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.import_queued", "")
                    Menu.Back()
            |> Some

        page_container()
        |+ PageButton(
            (if folder_detected then %"mount.create.use_detected_folder" else %"mount.create.game_not_detected"),
            (fun () -> FileDrop.on_file_drop.Value auto_detect_location),
            Disabled = K (not folder_detected)
        ).Pos(6)
        |+ Callout.frame info (fun (w, h) -> pretty_pos (0, 5, PageWidth.Custom w))
        :> Widget

    override this.Title = %"mount.create"

    override this.OnClose() = FileDrop.on_file_drop <- None