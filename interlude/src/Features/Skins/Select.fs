namespace Interlude.Features.Skins

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Skins
open Interlude.Content
open Interlude.Features.Online
open Interlude.Options
open Interlude.UI
open Interlude.Features.Import
open Interlude.Features.Gameplay
open Interlude.Features.Skins.EditNoteskin
open Interlude.Features.Skins.EditHUD
open Interlude.Features.Skins.Browser

type private NoteskinButton(id: string, meta: SkinMetadata, on_switch: unit -> unit, on_edit: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun _ ->
                Style.click.Play()
                if Skins.selected_noteskin_id.Value <> id then
                    options.Noteskin.Set id
                    on_switch ()
                else
                    on_edit()
            )
        )

    let credit =
        match meta.Editor with
        | Some e -> [meta.Author; e] %> "skins.credit.edited"
        | None -> [meta.Author] %> "skins.credit"

    member this.IsCurrent = Skins.selected_noteskin_id.Value = id

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K meta.Name,
            Color =
                (fun () ->
                    if this.Focused then Colors.text_yellow_2
                    elif this.IsCurrent then Colors.text_pink
                    else Colors.text
                ),
            Align = Alignment.LEFT,
            Position = Position.ShrinkL(70.0f).ShrinkX(Style.PADDING).SliceT(45.0f)
        )
        |+ Text(
            (fun () ->
                if this.Focused then
                    if this.IsCurrent then
                        if this.FocusedByMouse then %"skins.edit_hint_mouse" else [(%%"select").ToString()] %> "skins.edit_hint_keyboard"
                    else
                        if this.FocusedByMouse then %"skins.use_hint_mouse" else [(%%"select").ToString()] %> "skins.use_hint_keyboard"
                else credit
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.ShrinkL(70.0f).Shrink(Style.PADDING).SliceB(30.0f)
        )
        |* Clickable.Focus this

        match Skins.get_icon id with
        | Some sprite -> this.Add(Image(sprite, Position = Position.SliceL(70.0f).Shrink(Style.PADDING)))
        | None -> ()

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.IsCurrent then
            Render.rect this.Bounds Colors.pink_accent.O1
        elif this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

type private HUDButton(id: string, meta: SkinMetadata, on_switch: unit -> unit, on_edit: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun _ ->
                Style.click.Play()
                if Skins.selected_hud_id.Value <> id then
                    options.SelectedHUD.Set id
                    on_switch ()
                else
                    on_edit ()
            )
        )

    let credit =
        match meta.Editor with
        | Some e -> [meta.Author; e] %> "skins.credit.edited"
        | None -> [meta.Author] %> "skins.credit"

    member this.IsCurrent = Skins.selected_hud_id.Value = id

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K meta.Name,
            Color =
                (fun () ->
                    if this.Focused then Colors.text_yellow_2
                    elif this.IsCurrent then Colors.text_green
                    else Colors.text
                ),
            Align = Alignment.LEFT,
            Position = Position.ShrinkL(70.0f).ShrinkX(Style.PADDING).SliceT(45.0f)
        )
        |+ Text(
            (fun () ->
                if this.Focused then
                    if this.IsCurrent then
                        if this.FocusedByMouse then %"skins.edit_hint_mouse" else [(%%"select").ToString()] %> "skins.edit_hint_keyboard"
                    else
                        if this.FocusedByMouse then %"skins.use_hint_mouse" else [(%%"select").ToString()] %> "skins.use_hint_keyboard"
                else credit
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = Position.ShrinkL(70.0f).Shrink(Style.PADDING).SliceB(30.0f)
        )
        |* Clickable.Focus this

        match Skins.get_icon id with
        | Some sprite -> this.Add(Image(sprite, Position = Position.SliceL(70.0f).Shrink(Style.PADDING)))
        | None -> ()

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.IsCurrent then
            Render.rect this.Bounds Colors.green_accent.O1
        elif this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

type SelectSkinsPage() =
    inherit Page()

    let preview = SkinPreview(SkinPreview.LEFT_HAND_SIDE 0.35f)

    let noteskin_grid =
        GridFlowContainer<NoteskinButton>(70.0f, 1, WrapNavigation = false, Spacing = (20.0f, 20.0f))

    let hud_grid =
        GridFlowContainer<HUDButton>(70.0f, 1, WrapNavigation = false, Spacing = (20.0f, 20.0f))

    let edit_hud () =
        if
            SelectedChart.WITH_COLORS.IsSome
            && Screen.change_new
                (fun () -> EditHudScreen.edit_hud_screen (SelectedChart.CHART.Value, SelectedChart.WITH_COLORS.Value, fun () -> SelectSkinsPage().Show()))
                Screen.Type.Practice
                Transitions.Default
        then
            Menu.Exit()

    let edit_or_extract_noteskin () =
        let noteskin = Content.Noteskin

        if noteskin.IsEmbedded then
            ConfirmPage(
                %"skins.confirm_extract_default",
                (fun () ->
                    if
                        Skins.create_user_noteskin_from_default (
                            if Network.credentials.Username <> "" then
                                Some Network.credentials.Username
                            else
                                None
                        )
                        |> not
                    then
                        Logging.Error "An editable skin has already been extracted"
                )
            )
                .Show()
        else EditNoteskinPage().Show()

    let refresh () =
        preview.Refresh()

        noteskin_grid.Clear()
        for id, _, meta in Skins.list_noteskins () do
            noteskin_grid |* NoteskinButton(id, meta, preview.Refresh, edit_or_extract_noteskin)

        hud_grid.Clear()
        for id, _, meta in Skins.list_huds () do
            hud_grid |* HUDButton(id, meta, preview.Refresh, edit_hud)

    override this.Content() =
        refresh ()

        let left_info =
            NavigationContainer.Column(Position = { Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Right = 0.35f %- 10.0f })
            |+ OptionsMenuButton(
                Icons.DOWNLOAD_CLOUD + " " + %"skins.browser",
                0.0f,
                (fun () -> SkinsBrowserPage().Show()),
                Position = pretty_pos(PAGE_BOTTOM - 4, 2, PageWidth.Full).Translate(0.0f, -10.0f)
            )
            |+ OptionsMenuButton(
                Icons.DOWNLOAD + " " + %"skins.import_from_osu",
                0.0f,
                (fun () -> osu.Skins.OsuSkinsListPage().Show()),
                Position = pretty_pos(PAGE_BOTTOM - 2, 2, PageWidth.Full)
            )

        let noteskin_tab = ScrollContainer(noteskin_grid, Position = Position.SliceLPercent(0.5f).ShrinkT(110.0f).ShrinkR(Style.PADDING))
        let hud_tab = ScrollContainer(hud_grid, Position = Position.SliceRPercent(0.5f).ShrinkT(110.0f).ShrinkL(Style.PADDING))

        let right_side =
            NavigationContainer.Row(
                WrapNavigation = false,
                Position =
                    { Position.DEFAULT with
                        Left = 0.35f %+ 10.0f
                    }
                        .Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y)
            )
            |+ noteskin_tab
            |+ hud_tab
            |+ Text(
                %"skins.current_noteskin",
                Position = Position.SliceLPercent(0.5f).SliceT(PRETTYHEIGHT * 0.65f),
                Color = K Colors.text_subheading,
                Align = Alignment.LEFT
            )
            |+ Text(
                (fun () -> Content.NoteskinMeta.Name),
                Position = Position.SliceLPercent(0.5f).ShrinkT(PRETTYHEIGHT * 0.5f).SliceT(PRETTYHEIGHT),
                Color = K Colors.text,
                Align = Alignment.LEFT
            )
            |+ Text(
                %"skins.current_hud",
                Position = Position.SliceRPercent(0.5f).SliceT(PRETTYHEIGHT * 0.65f),
                Color = K Colors.text_subheading,
                Align = Alignment.LEFT
            )
            |+ Text(
                (fun () -> Content.HUDMeta.Name),
                Position = Position.SliceRPercent(0.5f).ShrinkT(PRETTYHEIGHT * 0.5f).SliceT(PRETTYHEIGHT),
                Color = K Colors.text,
                Align = Alignment.LEFT
            )

        NavigationContainer.Row()
        |+ right_side
        |+ left_info
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"skins"

    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = refresh()