﻿namespace Percyqaz.Flux.Input

open System
open OpenTK
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Graphics

type Keys = Windowing.GraphicsLibraryFramework.Keys
type MouseButton = Windowing.GraphicsLibraryFramework.MouseButton

[<RequireQualifiedAccess>]
type Bind =
    | Dummy
    | Key of Keys * modifiers: (bool * bool * bool)
    | Mouse of MouseButton
    // todo: joystick support ever?

    override this.ToString() =
        match this with
        | Dummy -> "NONE"
        | Key(k, m) -> Bind.ModifierString m + Bind.FormatKey k
        | Mouse b -> "M" + b.ToString()

    member this.WithModifiers (ctrl, alt, shift) : Bind =
        match this with
        | Dummy -> Dummy
        | Key (k, m) -> Key (k, (ctrl, alt, shift))
        | Mouse b -> Mouse b

    member this.IsKeyWithAnyModifiers (k: Keys) =
        match this with
        | Key (k1, _) when k1 = k -> true
        | _ -> false

    static member private FormatKey(k: Keys) : string =
        match k with
        | Keys.D0 -> "0"
        | Keys.D1 -> "1"
        | Keys.D2 -> "2"
        | Keys.D3 -> "3"
        | Keys.D4 -> "4"
        | Keys.D5 -> "5"
        | Keys.D6 -> "6"
        | Keys.D7 -> "7"
        | Keys.D8 -> "8"
        | Keys.D9 -> "9"
        | Keys.KeyPad0 -> "N0"
        | Keys.KeyPad1 -> "N1"
        | Keys.KeyPad2 -> "N2"
        | Keys.KeyPad3 -> "N3"
        | Keys.KeyPad4 -> "N4"
        | Keys.KeyPad5 -> "N5"
        | Keys.KeyPad6 -> "N6"
        | Keys.KeyPad7 -> "N7"
        | Keys.KeyPad8 -> "N8"
        | Keys.KeyPad9 -> "N9"
        | Keys.LeftAlt -> "LAlt"
        | Keys.RightAlt -> "RAlt"
        | Keys.LeftSuper -> "LSuper"
        | Keys.RightSuper -> "RSuper"
        | Keys.LeftControl -> "LCtrl"
        | Keys.RightControl -> "RCtrl"
        | Keys.LeftShift -> "LShift"
        | Keys.RightShift -> "RShift"
        | Keys.LeftBracket -> "["
        | Keys.RightBracket -> "]"
        | _ -> k.ToString()

    static member private ModifierString(ctrl, alt, shift) =
        (if ctrl then "Ctrl + " else "")
        + (if alt then "Alt + " else "")
        + (if shift then "Shift + " else "")

    static member internal IsModifier(k: Keys) =
        k = Keys.LeftShift
        || k = Keys.RightShift
        || k = Keys.LeftControl
        || k = Keys.RightControl
        || k = Keys.LeftAlt
        || k = Keys.RightAlt

module Bind =

    let inline mk k = Bind.Key(k, (false, false, false))
    let inline ctrl k = Bind.Key(k, (true, false, false))
    let inline alt k = Bind.Key(k, (false, true, false))
    let inline shift k = Bind.Key(k, (false, false, true))
    let inline ctrl_shift k = Bind.Key(k, (true, false, true))
    let inline ctrlAlt k = Bind.Key(k, (true, true, false))

type InputEvType = InputAction

type InputEv = (struct (Bind * InputEvType * float))

[<RequireQualifiedAccess>]
type InputListener =
    | Text of setting: Setting<string> * on_remove: (unit -> unit)
    | Bind of callback: (Bind -> unit)
    | None

type internal FrameEvents =
    {
        MouseX: float32
        MouseY: float32
        MouseZ: float32
        CursorInBounds: bool
        HeldKeys: Set<Keys>
        HeldMouseButtons: Set<MouseButton>
        Ctrl: bool
        Alt: bool
        Shift: bool
        TypedText: string
    }

module internal InputThread =

    let mutable private hide_cursor = false
    let mutable private cursor_in_bounds = true
    let mutable private mouse_x = 0.0f
    let mutable private mouse_y = 0.0f
    let mutable private mouse_z = 0.0f
    let mutable private typed_text = ""
    let mutable private events_buffer: InputEv list = []
    let mutable private ctrl = false
    let mutable private alt = false
    let mutable private shift = false
    let mutable private held_keys = Set.empty<Keys>
    let mutable private held_mouse_buttons = Set.empty<MouseButton>

    let mutable private typing = false
    let mutable private typing_buffered_input : InputEv voption = ValueNone

    let private LOCK_OBJ = Object()

    let private char_callback (_: nativeptr<Window>) (char: uint32) =
        if typing then
            lock LOCK_OBJ (fun () ->
                typed_text <- typed_text + Char.ConvertFromUtf32(int32 char)
                typing_buffered_input <- ValueNone
            )
    let private char_callback_d = GLFWCallbacks.CharCallback char_callback

    let private cursor_pos_callback (window: nativeptr<Window>) (x: float) (y: float) =
        lock LOCK_OBJ (fun () ->
            let start_x = (Render._framebuffer_width - Render._viewport_width) / 2 |> float32
            let pc_x = (float32 x - start_x) / float32 Render._viewport_width
            mouse_x <- Math.Clamp(Render._width * pc_x, 0.0f, Render._width)

            let start_y = (Render._framebuffer_height - Render._viewport_height) / 2 |> float32
            let pc_y = (float32 y - start_y) / float32 Render._viewport_height
            mouse_y <- Math.Clamp(Render._height * pc_y, 0.0f, Render._height)

            let was_in_bounds = cursor_in_bounds
            cursor_in_bounds <- pc_x >= 0.0f && pc_y >= 0.0f && pc_x <= 1.0f && pc_y <= 1.0f
            if cursor_in_bounds <> was_in_bounds && hide_cursor then
                GLFW.SetInputMode(window, CursorStateAttribute.Cursor, if cursor_in_bounds then CursorModeValue.CursorHidden else CursorModeValue.CursorNormal)
        )
    let private cursor_pos_callback_d = GLFWCallbacks.CursorPosCallback(cursor_pos_callback)

    let private scroll_callback (_: nativeptr<Window>) (offset_x: float) (offset_y: float) =
        lock LOCK_OBJ (fun () ->
            mouse_z <- mouse_z + float32 offset_y
        )
    let private scroll_callback_d = GLFWCallbacks.ScrollCallback(scroll_callback)

    let private key_callback (_: nativeptr<Window>) (key: Keys) (scancode: int) (action: InputAction) (modifiers: KeyModifiers) =
        let event =
            struct (
                (
                    key,
                    (
                        modifiers &&& KeyModifiers.Control = KeyModifiers.Control,
                        modifiers &&& KeyModifiers.Alt = KeyModifiers.Alt,
                        modifiers &&& KeyModifiers.Shift = KeyModifiers.Shift
                    )
                ) |> Bind.Key,
                action,
                GLFW.GetTime()
            )
        lock LOCK_OBJ (fun () ->
            if typing then
                typing_buffered_input <- ValueSome event
            else
                events_buffer <- List.append events_buffer [ event ]

            if action = InputAction.Release then
                if key = Keys.LeftControl || key = Keys.RightControl then ctrl <- false
                elif key = Keys.LeftAlt || key = Keys.RightAlt then alt <- false
                elif key = Keys.LeftShift || key = Keys.RightShift then shift <- false

                held_keys <- Set.remove key held_keys

            elif action = InputAction.Press then
                if key = Keys.LeftControl || key = Keys.RightControl then ctrl <- true
                elif key = Keys.LeftAlt || key = Keys.RightAlt then alt <- true
                elif key = Keys.LeftShift || key = Keys.RightShift then shift <- true

                held_keys <- Set.add key held_keys
        )
    let private key_callback_d = GLFWCallbacks.KeyCallback(key_callback)

    let private mouse_button_callback (_: nativeptr<Window>) (button: MouseButton) (action: InputAction) (modifiers: KeyModifiers) =
        let event =
            struct (
                Bind.Mouse button,
                action,
                GLFW.GetTime()
            )
        lock LOCK_OBJ (fun () ->
            events_buffer <- List.append events_buffer [ event ]
            if action = InputAction.Release then
                held_mouse_buttons <- Set.remove button held_mouse_buttons
            elif action = InputAction.Press then
                held_mouse_buttons <- Set.add button held_mouse_buttons
        )
    let private mouse_button_callback_d = GLFWCallbacks.MouseButtonCallback(mouse_button_callback)

    let enable_typing (v: bool) =
        lock LOCK_OBJ (fun () -> typing <- v; typing_buffered_input <- ValueNone)

    let set_cursor_hidden (b: bool) (window: nativeptr<Window>) =
        hide_cursor <- b
        if not b then
            GLFW.SetInputMode(window, CursorStateAttribute.Cursor, CursorModeValue.CursorNormal)
        elif OperatingSystem.IsMacOS() && GLFW.RawMouseMotionSupported() then
            GLFW.SetInputMode(window, RawMouseMotionAttribute.RawMouseMotion, true)
            GLFW.SetInputMode(window, CursorStateAttribute.Cursor, CursorModeValue.CursorDisabled)
        else
            GLFW.SetInputMode(window, CursorStateAttribute.Cursor, CursorModeValue.CursorHidden)

    let init (window: nativeptr<Window>) =
        GLFW.SetInputMode(window, LockKeyModAttribute.LockKeyMods, true);
        GLFW.SetCharCallback(window, char_callback_d) |> ignore
        GLFW.SetKeyCallback(window, key_callback_d) |> ignore
        GLFW.SetMouseButtonCallback(window, mouse_button_callback_d) |> ignore
        GLFW.SetScrollCallback(window, scroll_callback_d) |> ignore
        GLFW.SetCursorPosCallback(window, cursor_pos_callback_d) |> ignore

    let fetch (events_this_frame: InputEv list byref, this_frame: FrameEvents byref) =
        let a, b =
            lock
                LOCK_OBJ
                (fun () ->
                    let output =
                        {
                            MouseX = mouse_x
                            MouseY = mouse_y
                            MouseZ = mouse_z
                            CursorInBounds = cursor_in_bounds
                            TypedText = typed_text
                            HeldKeys = held_keys
                            HeldMouseButtons = held_mouse_buttons
                            Ctrl = ctrl
                            Shift = shift
                            Alt = alt
                        },
                        if typing then
                            match typing_buffered_input with
                            | ValueSome (struct (b, t, ts)) ->
                                if GLFW.GetTime() - ts > 0.002 then
                                    typing_buffered_input <- ValueNone
                                    (struct (b, t, ts)) :: events_buffer
                                else
                                    events_buffer
                            | _ -> events_buffer
                        else events_buffer

                    events_buffer <- []
                    typed_text <- ""
                    output
                )

        this_frame <- a
        events_this_frame <- b

module Input =

    let mutable last_time_mouse_moved = 0.0
    let mutable last_input_event = 0.0
    let mutable internal this_frame: FrameEvents = Unchecked.defaultof<_>
    let mutable internal last_frame: FrameEvents = Unchecked.defaultof<_>
    let mutable internal this_frame_finished = false

    let mutable internal events_this_frame: InputEv list = []

    let mutable internal input_listener: InputListener = InputListener.None
    let mutable internal input_listener_mouse_cancel = 0f

    let mutable internal scrolled_this_frame = 0f

    let mutable internal window: nativeptr<Window> = Unchecked.defaultof<_>

    /// Stops listening to text input OR to the next button pressed, if system previously was
    let remove_listener () =
        match input_listener with
        | InputListener.Text(_, on_remove) ->
            InputThread.enable_typing false
            on_remove ()
        | InputListener.Bind _
        | InputListener.None -> ()

        input_listener <- InputListener.None

    /// Used for UIs that let the user type some text into the provided text buffer
    /// In this mode keybindings do not activate (so something bound to A will not fire, it will type 'a' into the buffer instead)
    /// `mouse_cancel` set to true will cause the listener to automatically disconnect itself when the mouse is moved too much
    /// `on_remove` is called when the text listening mode is exited
    let listen_to_text (s: Setting<string>, mouse_cancel: bool, on_remove: unit -> unit) =
        remove_listener ()
        input_listener <- InputListener.Text(s, on_remove)
        InputThread.enable_typing true
        input_listener_mouse_cancel <- if mouse_cancel then 0f else -infinityf

    /// Used for UIs that let the user bind a key to a hotkey
    /// The very next button (or modifier + button combo) they press will be passed to `callback`
    /// Then the listener is removed
    let listen_to_next_key (callback: Bind -> unit) =
        remove_listener ()
        input_listener <- InputListener.Bind callback

    let pop_matching (b: Bind, t: InputEvType) =
        let mutable found = false

        let rec f evs =
            match evs with
            | [] -> []
            | struct (B, T, _) :: xs when B = b && T = t ->
                found <- true
                xs
            | x :: xs -> x :: (f xs)

        events_this_frame <- f events_this_frame
        found

    let pop_key_with_any_modifiers (k: Keys, t: InputEvType) =
        let mutable out = ValueNone

        let rec f evs =
            match evs with
            | [] -> []
            | struct (b: Bind, T, time) :: xs when T = t ->
                match b with
                | Bind.Key (k1, modifiers) when k1 = k ->
                    out <- ValueSome modifiers
                    xs
                | _ -> struct (b, T, time) :: f xs
            | x :: xs -> x :: f xs

        events_this_frame <- f events_this_frame
        out

    let pop_gameplay (now: Time) (binds: Bind array) (callback: int -> Time -> bool -> unit) =
        let glfw_now = GLFW.GetTime()

        let bind_match bind target =
            match bind, target with
            | Bind.Key(k, _), Bind.Key(K, _) when k = K -> true
            | Bind.Mouse b, Bind.Mouse B when b = B -> true
            | _ -> false

        let rec pop_inputs_matching_binds evs =
            match evs with
            | [] -> []
            | struct (b, t, time) :: xs when t <> InputEvType.Repeat ->
                let mutable i = 0
                let mutable matched = false

                while i < binds.Length && not matched do
                    if bind_match binds.[i] b then
                        callback i (now - float32 ((glfw_now - time) * 0.001) * 1.0f<ms>) (t <> InputEvType.Press)
                        matched <- true

                    i <- i + 1

                if matched then pop_inputs_matching_binds xs else struct (b, t, time) :: (pop_inputs_matching_binds xs)
            | _ :: xs -> pop_inputs_matching_binds xs

        events_this_frame <- pop_inputs_matching_binds events_this_frame

    let pop_any (t: InputEvType) =
        let mutable out = ValueNone

        let rec f evs =
            match evs with
            | [] -> []
            | struct (b, T, time) :: xs when T = t ->
                out <- ValueSome(b, time)
                xs
            | x :: xs -> x :: (f xs)

        events_this_frame <- f events_this_frame
        out

    let finish_frame_events () =
        input_listener_mouse_cancel <-
            input_listener_mouse_cancel
            + abs (this_frame.MouseX - last_frame.MouseX)
            + abs (this_frame.MouseY - last_frame.MouseY)

        last_frame <- this_frame
        events_this_frame <- []
        this_frame_finished <- true

    let key_held_any_modifiers (k: Keys) =
        this_frame.HeldKeys.Contains k

    let held (b: Bind) =
        if this_frame_finished then
            false
        else

        match b with
        | Bind.Key(Keys.LeftControl, _)
        | Bind.Key(Keys.RightControl, _) -> this_frame.Ctrl
        | Bind.Key(Keys.LeftAlt, _)
        | Bind.Key(Keys.RightAlt, _) -> this_frame.Alt
        | Bind.Key(Keys.LeftShift, _)
        | Bind.Key(Keys.RightShift, _) -> this_frame.Shift
        | Bind.Key(k, m) ->
            this_frame.HeldKeys.Contains k
            && m = (this_frame.Ctrl, this_frame.Alt, this_frame.Shift)
        | Bind.Mouse m -> this_frame.HeldMouseButtons.Contains m
        | Bind.Dummy -> false

    let init (_window: nativeptr<Window>) =
        window <- _window
        InputThread.init window

    let private DELETE_CHARACTER = Bind.mk Keys.Backspace
    let private DELETE_WORD = Bind.ctrl Keys.Backspace
    let private COPY = Bind.ctrl Keys.C
    let private PASTE = Bind.ctrl Keys.V

    let update_input_listener () =

        match input_listener with
        | InputListener.Text(s, _) ->

            if this_frame.TypedText <> "" then
                s.Value <- s.Value + this_frame.TypedText

            if s.Value.Length > 0 && (pop_matching(DELETE_CHARACTER, InputEvType.Press) || pop_matching(DELETE_CHARACTER, InputEvType.Repeat)) then
                Setting.app (fun (x: string) -> x.Substring(0, x.Length - 1)) s

            elif s.Value.Length > 0 && (pop_matching(DELETE_WORD, InputEvType.Press) || pop_matching(DELETE_WORD, InputEvType.Repeat)) then
                s.Value <-
                    let parts = s.Value.Split(" ")
                    Array.take (parts.Length - 1) parts |> String.concat " "

            elif pop_matching(COPY, InputEvType.Press) then
                GLFW.SetClipboardString(window, s.Value)

            elif pop_matching(PASTE, InputEvType.Press) then
                s.Value <-
                    s.Value
                    + try
                        GLFW.GetClipboardString(window)
                      with _ ->
                          ""

            if input_listener_mouse_cancel > 200f then
                remove_listener ()

        | InputListener.Bind callback ->
            match pop_any InputEvType.Press with
            | ValueSome(x, _) ->
                match x with
                | Bind.Key(k, m) ->
                    if Bind.IsModifier k then
                        ()
                    else
                        remove_listener ()
                        callback x
                | _ ->
                    remove_listener ()
                    callback x
            | ValueNone ->
                match pop_any InputEvType.Release with
                | ValueSome(x, _) ->
                    match x with
                    | Bind.Key(k, m) ->
                        if Bind.IsModifier k then
                            remove_listener ()
                            callback x
                    | _ -> ()
                | ValueNone -> ()

        | InputListener.None -> ()

    let begin_frame_events () =
        last_frame <- this_frame
        InputThread.fetch (&events_this_frame, &this_frame)

        if Object.ReferenceEquals(null, last_frame) then
            last_frame <- this_frame

        if
            (this_frame.MouseX <> last_frame.MouseX)
            || (this_frame.MouseY <> last_frame.MouseY)
        then
            last_time_mouse_moved <- GLFW.GetTime()

        if events_this_frame <> [] then
            last_input_event <- GLFW.GetTime()

        scrolled_this_frame <- this_frame.MouseZ - last_frame.MouseZ
        this_frame_finished <- false
        update_input_listener ()

    let button_pressed_recently () = GLFW.GetTime() - last_input_event < 0.100

module Mouse =

    let LEFT = MouseButton.Left
    let RIGHT = MouseButton.Right
    let MIDDLE = MouseButton.Middle

    let pos () =
        (Input.this_frame.MouseX, Input.this_frame.MouseY)

    let in_bounds () = Input.this_frame.CursorInBounds

    let x () = fst (pos ())
    let y () = snd (pos ())

    let scroll () =
        let v = Input.scrolled_this_frame
        Input.scrolled_this_frame <- 0.0f
        v

    let private click b : bool =
        Input.pop_matching(Bind.Mouse b, InputEvType.Press)

    let left_click () : bool = click LEFT
    let right_click () : bool = click RIGHT
    let middle_click () : bool = click MIDDLE

    let held b : bool = Input.held (Bind.Mouse b)

    let released b : bool =
        Input.pop_matching(Bind.Mouse b, InputEvType.Release)

    let moved_recently () : bool = GLFW.GetTime() - Input.last_time_mouse_moved < 0.100

    let hover (r: Rect) : bool =
        not Input.this_frame_finished && r.Contains(pos ())

type Bind with
    member this.Pressed() =
        match this with
        | Key _
        | Mouse _ -> Input.held this
        | _ -> false

    member this.Repeated() =
        match this with
        | Key _
        | Mouse _ -> Input.pop_matching(this, InputEvType.Repeat)
        | _ -> false

    member this.TappedOrRepeated() =
        match this with
        | Key _
        | Mouse _ ->
            Input.pop_matching(this, InputEvType.Press)
            || Input.pop_matching(this, InputEvType.Repeat)
        | _ -> false

    member this.Tapped() =
        match this with
        | Key _
        | Mouse _ -> Input.pop_matching(this, InputEvType.Press)
        | _ -> false

    member this.Released() =
        match this with
        | Key _
        | Mouse _ -> Input.pop_matching(this, InputEvType.Release)
        | _ -> false