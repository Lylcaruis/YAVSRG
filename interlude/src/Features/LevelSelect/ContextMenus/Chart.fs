﻿namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Charts
open Prelude.Backbeat
open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Collections
open Interlude.Features.Tables
open Interlude.Features.Play
open Interlude.Features.Export

type ChartDeleteMenu(cc: ChartMeta, context: LibraryContext, is_submenu: bool) =
    inherit Page()

    let delete_from_everywhere() =
        ChartDatabase.delete cc Content.Charts
        LevelSelect.refresh_all ()

        if is_submenu then
            Menu.Back()

    override this.Content() =
        page_container()
        |+ seq {
            match context with
            | LibraryContext.Pack p when cc.Packs.Count > 1 ->

                let delete_from_pack() =
                    ChartDatabase.delete_from_pack cc p Content.Charts
                    LevelSelect.refresh_all ()

                    if is_submenu then
                        Menu.Back()

                yield PageButton.Once([p] %> "chart.delete.from_pack", fun () -> delete_from_pack(); Menu.Back()).Pos(3)
                yield PageButton.Once([cc.Packs.Count.ToString()] %> "chart.delete.from_everywhere", fun () -> delete_from_everywhere(); Menu.Back()).Pos(5)
                yield PageButton.Once(%"confirm.no", Menu.Back).Pos(7)
            | _ ->
                yield PageButton.Once(
                    (if cc.Packs.Count > 1 then [cc.Packs.Count.ToString()] %> "chart.delete.from_everywhere" else %"confirm.yes"),
                    fun () -> delete_from_everywhere(); Menu.Back()).Pos(3)
                yield PageButton.Once(%"confirm.no", Menu.Back).Pos(5)
        }
        |+ Text([ sprintf "%s [%s]" cc.Title cc.DifficultyName ] %> "misc.confirmdelete", Align = Alignment.LEFT, Position = pretty_pos(0, 2, PageWidth.Full))
        :> Widget

    override this.Title = %"chart.delete"
    override this.OnClose() = ()

#nowarn "40"

type ChartContextMenu(cc: ChartMeta, context: LibraryContext) =
    inherit Page()

    let rec like_button =
        PageButton(
            %"chart.add_to_likes",
            (fun () -> CollectionActions.toggle_liked cc; like_button_swap.Current <- unlike_button),
            Icon = Icons.HEART,
            Hotkey = %%"like"
        )
    and unlike_button =
        PageButton(
            %"chart.remove_from_likes",
            (fun () -> CollectionActions.toggle_liked cc; like_button_swap.Current <- like_button),
            Icon = Icons.FOLDER_MINUS,
            Hotkey = %%"like"
        )
    and like_button_swap : SwapContainer = SwapContainer(if CollectionActions.is_liked cc then unlike_button else like_button)

    override this.Content() =
        let content =
            FlowContainer.Vertical<Widget>(PRETTYHEIGHT, Position = pretty_pos(0, PAGE_BOTTOM, PageWidth.Normal).Translate(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
            |+ like_button_swap
            |+ PageButton(
                %"chart.add_to_collection",
                (fun () -> AddToCollectionPage(cc).Show()),
                Icon = Icons.FOLDER_PLUS
            )
            |+ PageButton(%"chart.delete", (fun () -> ChartDeleteMenu(cc, context, true).Show()), Hotkey = %%"delete", Icon = Icons.TRASH)

        match context with
        | LibraryContext.None
        | LibraryContext.Likes
        | LibraryContext.Pack _
        | LibraryContext.Table _ -> ()
        | LibraryContext.Folder name ->
            content
            |* PageButton(
                [ name ] %> "chart.remove_from_collection",
                (fun () ->
                    if CollectionActions.remove_from (name, Content.Collections.Get(name).Value, cc, context) then
                        Menu.Back()
                ),
                Icon = Icons.FOLDER_MINUS
            )
        | LibraryContext.Playlist(index, name, _) ->
            content
            |+ PageButton(
                %"chart.move_up_in_playlist",
                (fun () ->
                    if CollectionActions.reorder_up context then
                        Menu.Back()
                ),
                Icon = Icons.ARROW_UP_CIRCLE,
                Disabled = K (index = 0),
                Hotkey = %%"move_up_in_playlist"
            )
            |+ PageButton(
                %"chart.move_down_in_playlist",
                (fun () ->
                    if CollectionActions.reorder_down context then
                        Menu.Back()
                ),
                Icon = Icons.ARROW_DOWN_CIRCLE,
                Disabled = K (index + 1 = Content.Collections.GetPlaylist(name).Value.Charts.Count),
                Hotkey = %%"move_down_in_playlist"
            )
            |+ PageButton(
                [ name ] %> "chart.remove_from_collection",
                (fun () ->
                    if CollectionActions.remove_from (name, Content.Collections.Get(name).Value, cc, context) then
                        Menu.Back()
                ),
                Icon = Icons.FOLDER_MINUS
            )
            |+ PageButton.Once(
                %"playlist.play",
                (fun () ->
                    LevelSelect.start_playlist (name, Content.Collections.GetPlaylist(name).Value)
                ),
                Icon = Icons.PLAY,
                Disabled = K Network.lobby.IsSome
            )
            |* PageButton.Once(
                %"playlist.play_shuffled",
                (fun () ->
                    LevelSelect.start_playlist_shuffled (name, Content.Collections.GetPlaylist(name).Value)
                ),
                Icon = Icons.SHUFFLE,
                Disabled = K Network.lobby.IsSome
            )

        if Some cc = SelectedChart.CACHE_DATA then
            content
            |+ PageButton(%"chart.change_offset",
                fun () ->
                    match SelectedChart.CHART, SelectedChart.SAVE_DATA with
                    | Some chart, Some save_data ->
                        LocalOffsetPage(LocalOffset.get_recent_suggestion chart save_data, LocalOffset.offset_setting save_data, ignore)
                            .Show()
                    | _ -> ()
                , Icon = Icons.SPEAKER
            )
            |+ PageButton.Once(
                %"chart.practice",
                (fun () ->
                    Menu.Exit()
                    SelectedChart.when_loaded
                        true
                        (fun info ->
                            Screen.change_new
                                (fun () -> PracticeScreen.practice_screen (info, 0.0f<ms>))
                                ScreenType.Practice
                                Transitions.Default
                            |> ignore
                        )
                ),
                Icon = Icons.TARGET,
                Hotkey = %%"practice_mode"
            )
            |* PageButton.Once(
                %"chart.export_osz",
                (fun () ->
                    match SelectedChart.CHART, SelectedChart.WITH_MODS with
                    | Some c, Some m ->
                        OsuExportOptionsPage(
                            %"chart.export_osz",
                            m.ModsApplied,
                            function
                            | true -> OsuExport.export_chart_with_mods m cc
                            | false -> OsuExport.export_chart_without_mods c cc
                        )
                            .Show()
                    | _ -> ()
                ),
                Icon = Icons.UPLOAD
            )

        content

    override this.Title = cc.Title
    override this.OnClose() = ()