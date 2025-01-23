namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Options
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Collections
open Interlude.Features.Tables

type private ModeDropdown
    (options: (string * string) seq, label: string, setting: Setting<string>, reverse: Setting<bool>, bind: Hotkey) =
    inherit Container(NodeType.None)

    let LEFT_PERCENT = 0.4f

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
            Position = { Position.DEFAULT with Right = LEFT_PERCENT %+ 0.0f }
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
            Position = { Position.DEFAULT with Left = LEFT_PERCENT %+ 25.0f }
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

type LibraryViewControls() =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        this
        |+ StylishButton(
            (fun () -> LevelSelectOptionsPage().Show()),
            K Icons.SETTINGS,
            !%Palette.DARK_100,
            Hotkey = "level_select_options",
            Position =
                {
                    Left = 0.4f %+ 25.0f
                    Top = 0.0f %+ 120.0f
                    Right = 0.4f %+ 85.0f
                    Bottom = 0.0f %+ 170.0f
                }
        )

        |+ ModeDropdown(
            Sorting.modes.Keys
            |> Seq.map (fun id -> (id, Localisation.localise (sprintf "levelselect.sortby." + id))),
            "Sort",
            options.ChartSortMode |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            options.ChartSortReverse
            |> Setting.map not not
            |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            "sort_mode",
            Position =
                {
                    Left = 0.4f %+ 110.0f
                    Top = 0.0f %+ 120.0f
                    Right = 0.7f %+ 30.0f
                    Bottom = 0.0f %+ 170.0f
                }
        )
            .Help(
                Help
                    .Info("levelselect.sortby", "sort_mode")
                    .Hotkey(%"levelselect.sortby.reverse_hint", "reverse_sort_mode")
            )

        |* ModeDropdown(
            Grouping.modes.Keys
            |> Seq.map (fun id -> (id, Localisation.localise (sprintf "levelselect.groupby." + id))),
            "Group",
            options.ChartGroupMode |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            options.ChartGroupReverse |> Setting.trigger (ignore >> LevelSelect.refresh_all),
            "group_mode",
            Position =
                {
                    Left = 0.7f %+ 55.0f
                    Top = 0.0f %+ 120.0f
                    Right = 1.0f %+ 0.0f
                    Bottom = 0.0f %+ 170.0f
                }
        )
            .Help(
                Help
                    .Info("levelselect.groupby", "group_mode")
                    .Hotkey(%"levelselect.groupby.reverse_hint", "reverse_group_mode")
            )

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if SelectedChart.CACHE_DATA.IsSome then
            if (%%"move_up_in_playlist").Tapped() then
                CollectionActions.reorder_up SelectedChart.LIBRARY_CTX |> ignore // todo: play sound effect
            elif (%%"move_down_in_playlist").Tapped() then
                CollectionActions.reorder_down SelectedChart.LIBRARY_CTX |> ignore
            elif (%%"like").Tapped() then
                CollectionActions.toggle_liked SelectedChart.CACHE_DATA.Value

            //elif (%%"skip").Tapped() then
            //    FiltersPage().Show()

            elif (%%"collections").Tapped() then
                ManageCollectionsPage().Show()
            elif (%%"table").Tapped() then
                SelectTablePage(LevelSelect.refresh_all).Show()
            elif (%%"reverse_sort_mode").Tapped() then
                Setting.app not options.ChartSortReverse
                LevelSelect.refresh_all ()
            elif (%%"reverse_group_mode").Tapped() then
                Setting.app not options.ChartGroupReverse
                LevelSelect.refresh_all ()