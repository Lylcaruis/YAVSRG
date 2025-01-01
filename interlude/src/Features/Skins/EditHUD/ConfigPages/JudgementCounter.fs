namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay.Rulesets
open Interlude.Content
open Interlude.UI
open Interlude.Features.Play.HUD

type private DisplayPicker(ruleset: Ruleset, i: int, data: int option array) =
    inherit Container(NodeType.Leaf)

    let texture = Content.Texture "judgement-counter-judgements"

    let fd () =
        data.[i] <-
            match data.[i] with
            | None ->
                Some 0
            | Some i ->
                if i + 1 >= texture.Rows then 
                    None
                else Some (i + 1)
        Style.click.Play()

    let bk () =
        data.[i] <-
            match data.[i] with
            | None ->
                Some (texture.Rows - 1)
            | Some 0 ->
               None 
            | Some i -> 
                Some (i - 1)
        Style.click.Play()

    override this.Init(parent: Widget) =
        this
        |* Clickable(
            (fun () ->
                this.Select true
                fd ()
            ),
            OnHover =
                fun b ->
                    if b && not this.Focused then
                        this.Focus true
                    elif not b && this.FocusedByMouse then
                        Selection.up true
        )

        base.Init parent

    override this.Draw() =

        if this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O2
            if this.Selected then
                Render.rect (this.Bounds.SlicePercentR(0.5f).SliceB(5.0f).Shrink(100.0f, 0.0f)) Colors.yellow_accent

        Text.fill(Style.font, ruleset.JudgementName i, this.Bounds.SlicePercentL(0.5f).Shrink(10.0f, 5.0f), ruleset.JudgementColor i, Alignment.CENTER)
        Text.fill_b(Style.font, Icons.ARROW_RIGHT, this.Bounds.Shrink(10.0f, 5.0f), Colors.text_greyout, Alignment.CENTER)

        match data.[i] with
        | None ->
            Text.fill_b(Style.font, ruleset.JudgementName i, this.Bounds.SlicePercentR(0.5f).Shrink(10.0f, 5.0f), (Color.White, Color.Black), Alignment.CENTER)
        | Some j ->
            Render.tex_quad (Sprite.fill (this.Bounds.SlicePercentR(0.5f).Shrink(10.0f, 5.0f)) texture).AsQuad Color.White.AsQuad (Sprite.pick_texture (0, j) texture)

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if this.Selected then
            if (%%"left").Tapped() then
                bk ()
            elif (%%"right").Tapped() then
                fd ()
            elif (%%"up").Tapped() then
                fd ()
            elif (%%"down").Tapped() then
                bk ()

type private JudgementCounterDisplayPage(use_texture: Setting<bool>, display: int option array, ruleset: Ruleset) =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageSetting(
            %"hud.judgement_counter.usetextures",
            Checkbox use_texture
        )
            .Help(Help.Info("hud.judgement_counter.usetextures"))
            .Pos(0)
        |+ Conditional(use_texture.Get,
            FlowContainer.Vertical<Widget>(PRETTYHEIGHT)
            |+ seq {
                for i = 0 to ruleset.Judgements.Length - 1 do
                    yield DisplayPicker(ruleset, i, display)
            }
            |> ScrollContainer,
            Position = pretty_pos(2, PAGE_BOTTOM - 10, PageWidth.Normal)
        )
        :> Widget

    override this.Title = %"hud.judgement_counter.textures"
    override this.OnClose() = ()

type JudgementCounterPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD

    let pos = Setting.simple config.JudgementCounterPosition

    let animation_time =
        config.JudgementCounterFadeTime
        |> Setting.bounded (100.0f<ms / rate>, 1000.0f<ms / rate>)
    let show_ratio = Setting.simple config.JudgementCounterShowRatio

    let use_background = Setting.simple config.JudgementCounterBackground.Enable
    let background_scale = config.JudgementCounterBackground.Scale |> Setting.bounded (0.5f, 2.0f)
    let background_offset_x = Setting.percentf config.JudgementCounterBackground.AlignmentX
    let background_offset_y = Setting.percentf config.JudgementCounterBackground.AlignmentY

    let use_font = Setting.simple config.JudgementCounterUseFont
    let font_spacing = config.JudgementCounterFontSpacing |> Setting.bounded (-1.0f, 1.0f)
    let font_dot_spacing = config.JudgementCounterDotExtraSpacing |> Setting.bounded (-1.0f, 1.0f)
    let font_colon_spacing = config.JudgementCounterColonExtraSpacing |> Setting.bounded (-1.0f, 1.0f)

    let texture = Content.Texture "judgement-counter-judgements"
    let use_texture = Setting.simple config.JudgementCounterUseJudgementTextures
    let ruleset = Rulesets.current
    let JUDGEMENT_COUNT = ruleset.Judgements.Length
    let display : int option array = 
        match config.JudgementCounterCustomDisplay.TryFind JUDGEMENT_COUNT with
        | Some existing -> Array.copy existing
        | None -> 
            if texture.Rows = JUDGEMENT_COUNT then
                Array.init JUDGEMENT_COUNT Some
            else
                Array.create JUDGEMENT_COUNT None

    let font_texture = Content.Texture "judgement-counter-font"

    let preview =
        { new ConfigPreviewNew(pos.Value) with
            override this.DrawComponent(bounds) =
                let h = bounds.Height / float32 (ruleset.Judgements.Length + if show_ratio.Value then 1 else 0)
                let mutable r = bounds.SliceT(h)

                for i = 0 to ruleset.Judgements.Length - 1 do
                    let j = ruleset.Judgements.[i]

                    match display.[i] with
                    | Some texture_index ->
                        Render.tex_quad 
                                ((Sprite.fill_left (r.Shrink(5.0f)) texture).AsQuad)
                                Color.White.AsQuad
                                (Sprite.pick_texture (0, texture_index) texture)
                    | None ->
                        Render.rect (r.SliceL(5.0f)) j.Color
                        Text.fill_b (Style.font, j.Name, r.Shrink(10.0f, 5.0f), (Color.White, Color.Black), Alignment.LEFT)

                    if use_font.Value then
                        JudgementCounter.draw_count_right_aligned(font_texture, r.Shrink(5.0f), Color.White, 730 - i * 7, font_spacing.Value)
                    else
                        Text.fill_b (
                            Style.font,
                            (730 - i * 7).ToString(),
                            r.Shrink(5.0f),
                            (Color.White, Color.Black),
                            Alignment.RIGHT
                        )

                    r <- r.Translate(0.0f, h)

                if show_ratio.Value then
                    if use_font.Value then
                        JudgementCounter.draw_ratio_centered(
                            font_texture,
                            r.Shrink(5.0f),
                            Color.White,
                            (730, 727),
                            font_spacing.Value,
                            font_dot_spacing.Value,
                            font_colon_spacing.Value
                        )
                    else
                        Text.fill_b (
                            Style.font,
                            sprintf "%.2f:1" (730.0 / 727.0),
                            r.Shrink(5.0f),
                            (Color.White, Color.Black),
                            Alignment.CENTER
                        )
        }

    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.judgement_counter.animationtime", Slider(Setting.uom animation_time, Step = 5f))
            .Help(Help.Info("hud.judgement_counter.animationtime"))
            .Pos(0)
        |+ PageSetting(%"hud.judgement_counter.showratio", Checkbox show_ratio)
            .Help(Help.Info("hud.judgement_counter.showratio"))
            .Pos(2)
        |+ PageSetting(%"hud.generic.use_font", Checkbox use_font)
            .Help(Help.Info("hud.generic.use_font"))
            .Pos(4)
        |+ PageSetting(%"hud.generic.font_spacing", Slider.Percent(font_spacing))
            .Help(Help.Info("hud.generic.font_spacing"))
            .Pos(6)
            .Conditional(use_font.Get)
        |+ PageSetting(%"hud.generic.dot_spacing", Slider.Percent(font_dot_spacing))
            .Help(Help.Info("hud.generic.dot_spacing"))
            .Pos(8)
            .Conditional(use_font.Get)
        |+ PageSetting(%"hud.generic.colon_spacing", Slider.Percent(font_colon_spacing))
            .Help(Help.Info("hud.generic.colon_spacing"))
            .Pos(10)
            .Conditional(use_font.Get)
        |+ PageButton(%"hud.judgement_counter.textures", 
            fun () -> JudgementCounterDisplayPage(use_texture, display, ruleset).Show())
            .Pos(12)
        |+ PageSetting(%"hud.judgement_counter.usebackground", Checkbox use_background)
            .Help(Help.Info("hud.judgement_counter.usebackground"))
            .Pos(14)
        |+ PageSetting(%"hud.judgement_counter.backgroundscale", Slider.Percent(background_scale))
            .Help(Help.Info("hud.judgement_counter.backgroundscale"))
            .Pos(16)
            .Conditional(use_background.Get)
        |+ PageSetting(%"hud.judgement_counter.background_offset_x", Slider.Percent(background_offset_x))
            .Help(Help.Info("hud.judgement_counter.background_offset_x"))
            .Pos(18)
            .Conditional(use_background.Get)
        |+ PageSetting(%"hud.judgement_counter.background_offset_y", Slider.Percent(background_offset_y))
            .Help(Help.Info("hud.judgement_counter.background_offset_y"))
            .Pos(20)
            .Conditional(use_background.Get)
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"hud.judgement_counter"

    override this.OnClose() =
        Skins.save_hud_config 
            { Content.HUD with
                JudgementCounterFadeTime = animation_time.Value
                JudgementCounterShowRatio = show_ratio.Value
                JudgementCounterBackground = 
                    {
                        Enable = use_background.Value
                        Scale = background_scale.Value
                        AlignmentX = background_offset_x.Value
                        AlignmentY = background_offset_y.Value
                    }
                JudgementCounterUseFont = use_font.Value
                JudgementCounterFontSpacing = font_spacing.Value
                JudgementCounterDotExtraSpacing = font_dot_spacing.Value
                JudgementCounterColonExtraSpacing = font_colon_spacing.Value

                JudgementCounterUseJudgementTextures = use_texture.Value
                JudgementCounterCustomDisplay = Content.HUD.JudgementCounterCustomDisplay.Add (JUDGEMENT_COUNT, display)
            }

        on_close ()