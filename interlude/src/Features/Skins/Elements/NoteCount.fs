namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Play
open Prelude.Charts
open Interlude.Features.Gameplay.SelectedChart

module NoteCount =

    let draw_notecount_centered (texture: Sprite, bounds: Rect, color: Color, combo: int, spacing: float32) =
        let notecount_text = Interlude.Features.Gameplay.SelectedChart.FMT_NOTECOUNTS.Value
        let char_width = float32 texture.Width
        let width = (float32 notecount_text.Length + (float32 notecount_text.Length - 1.0f) * spacing) * char_width
        let height = float32 texture.Height
        let scale = min (bounds.Width / width) (bounds.Height / height)

        let mutable char_bounds =
            Rect.FromSize(
                bounds.CenterX - width * scale * 0.5f,
                bounds.CenterY - height * scale * 0.5f,
                char_width * scale,
                height * scale
            )

        for c in notecount_text do
            Render.tex_quad char_bounds.AsQuad color.AsQuad (Sprite.pick_texture (0, int (c - '0')) texture)
            char_bounds <- char_bounds.Translate(scale * (1.0f + spacing) * char_width, 0.0f)

type Notecount(config: HudConfig, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let pop_animation = Animation.Fade(0.0f)
    let color = Animation.Color(Color.White)
    let mutable event_count = 0

    let font_texture = Content.Texture "combo-font"

    do
        state.SubscribeEvents(fun ev ->
            match ev.Combo with
            | NoChange -> ()
            | _ ->

            event_count <- event_count + 1

            if (config.ComboLampColors && event_count > 50) then
                color.Target <-
                    Lamp.calculate state.Ruleset.Lamps state.Scoring.JudgementCounts state.Scoring.ComboBreaks
                    |> state.Ruleset.LampColor

            pop_animation.Value <- config.ComboPop
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        color.Update elapsed_ms

    override this.Draw() =
        let combo = state.Scoring.CurrentCombo

        let amt = 1f
         //   pop_animation.Value
          //  + (((combo, 1000) |> Math.Min |> float32) * config.ComboGrowth)

        let bounds = this.Bounds.Expand amt

        //if config.NotecountUseFont then
            //Notecount.draw_notecount_centered(font_texture, bounds, color.Value, combo, config.NotecountFontSpacing)
        //else
        Text.fill (Style.font, "e", bounds, Colors.white, 0.5f) // Interlude.Features.Gameplay.SelectedChart.FMT_NOTECOUNTS.Value