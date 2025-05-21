namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI

type NotecountPage(on_close: unit -> unit) = 
    inherit Page()

    let config = Content.HUD
    override this.Content() =
        page_container()

        :> Widget
    override this.Title = %"hud.notecount"
    override this.OnClose() =
         on_close ()
