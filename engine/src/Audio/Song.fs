﻿namespace Percyqaz.Flux.Audio

open System
open System.Diagnostics
open ManagedBass
open ManagedBass.Fx
open Percyqaz.Common

type Song =
    {
        ID: int
        Frequency: int // in hz
        Duration: Time
        PreviewPoint: Time
        LastNote: Time
        mutable LowPassFilterEffect: int
    }
    static member Default =
        {
            ID = 0
            Frequency = 1
            Duration = 1000.0f<ms>
            PreviewPoint = 0.0f<ms>
            LastNote = 1000.0f<ms>
            LowPassFilterEffect = 0
        }

    static member FromFile(preview_point: Time, last_note: Time, file: string) =
        let ID = Bass.CreateStream(file, 0L, 0L, BassFlags.Prescan ||| BassFlags.Decode)

        if ID = 0 then
            Logging.Error "Couldn't load audio track from '%s': %s"
                file
                (match Bass.LastError with Errors.FileOpen -> "File couldn't be found/opened OR contains an audio format not supported" | other -> other.ToString())
            { Song.Default with PreviewPoint = preview_point; LastNote = last_note }
        else
            let d = Bass.ChannelGetInfo ID
            let Duration = Bass.ChannelBytes2Seconds(ID, Bass.ChannelGetLength ID) |> float32 |> (*) 1000.0f<ms>
            let Frequency = d.Frequency
            let ID = BassFx.TempoCreate(ID, BassFlags.FxFreeSource)
            Bass.ChannelSetDevice(ID, Bass.CurrentDevice) |> display_bass_error
            Bass.ChannelSetAttribute(ID, ChannelAttribute.NoBuffer, 1f) |> display_bass_error
            Bass.ChannelSetAttribute(ID, ChannelAttribute.TempoUseQuickAlgorithm, 1) |> display_bass_error
            Bass.ChannelSetAttribute(ID, ChannelAttribute.TempoOverlapMilliseconds, 4) |> display_bass_error
            Bass.ChannelSetAttribute(ID, ChannelAttribute.TempoSequenceMilliseconds, 30) |> display_bass_error

            {
                ID = ID
                Frequency = Frequency
                Duration = Duration
                PreviewPoint = if preview_point >= Duration - 3000.0f<ms> || preview_point < 0.0f<ms> then Duration * 0.5f else preview_point
                LastNote = last_note
                LowPassFilterEffect = 0
            }

    member this.Free() =
        Bass.StreamFree this.ID |> display_bass_error

    member this.SetLowPass(amount: float32) =
        match this.LowPassFilterEffect with
        | 0 when amount > 0.0f ->
            let effect = Bass.ChannelSetFX(this.ID, EffectType.BQF, 1)
            let center = lerp amount 22049f 800f
            Bass.FXSetParameters(effect, BQFParameters(lFilter = BQFType.LowPass, fCenter = center, fBandwidth = 0f, fQ = 0.7f)) |> display_bass_error
            this.LowPassFilterEffect <- effect
        | existing when amount = 0.0f ->
            Bass.ChannelRemoveFX(this.ID, existing) |> display_bass_error
            this.LowPassFilterEffect <- 0
        | existing ->
            let center = lerp amount 22049f 800f
            Bass.FXSetParameters(existing, BQFParameters(lFilter = BQFType.LowPass, fCenter = center, fBandwidth = 0f, fQ = 0.7f)) |> display_bass_error

[<RequireQualifiedAccess>]
type SongFinishAction =
    | LoopFromPreview
    | LoopFromBeginning
    | Wait
    | Custom of (unit -> unit)

[<RequireQualifiedAccess>]
type SongLoadAction =
    | PlayFromPreview
    | PlayFromBeginning
    | Wait

module Song =

    let LEADIN_TIME = 3000.0f<ms / rate>
    let CROSSFADE_DURATION_MS = 200

    let private on_loaded_ev = Event<unit>()
    let on_loaded = on_loaded_ev.Publish

    let mutable private load_path: string option = None
    let mutable loading = false
    let mutable internal now_playing: Song = Song.Default

    let mutable private _time = 0.0f<ms>
    let private timer = new Stopwatch()
    let mutable private timer_start = 0.0f<ms>
    let mutable private channel_playing = false
    let mutable private paused = false
    let mutable on_finish = SongFinishAction.Wait
    let mutable internal seek_inaccuracy_compensation = 37.0f<ms>

    let mutable private rate = 1.0f<rate>
    let mutable private enable_pitch_rates = true

    let mutable private _local_offset = 0.0f<ms>
    let mutable private _global_offset = 0.0f<ms / rate>

    let mutable private low_pass_amount = 0.0f
    let mutable private low_pass_target = 0.0f

    let duration () = now_playing.Duration

    let private update_time() =
        _time <- rate * (float32 timer.Elapsed.TotalMilliseconds * 1.0f<ms / rate>) + timer_start

    let frame_compensation () =
        (rate * (float32 timer.Elapsed.TotalMilliseconds * 1.0f<ms / rate>) + timer_start) - _time

    let time () = _time

    let time_compensated () =
        time () - seek_inaccuracy_compensation

    let time_with_offset () =
        time_compensated () + _local_offset + _global_offset * rate

    let playing () = timer.IsRunning

    let play_from (time) =

        if time >= 0.0f<ms> && time < duration () then

            Bass.ChannelSetPosition(
                now_playing.ID,
                Bass.ChannelSeconds2Bytes(now_playing.ID, float <| time / 1000.0f<ms>)
            )
            |> display_bass_error

            Bass.ChannelPlay now_playing.ID |> display_bass_error
            channel_playing <- true

        else if channel_playing then
            Bass.ChannelStop now_playing.ID |> display_bass_error
            channel_playing <- false

        timer_start <- time
        timer.Restart()
        paused <- false
        update_time()

    let play_leadin (first_note: Time) =
        play_from (min 0.0f<ms> (first_note - LEADIN_TIME * rate))

    let seek (time) =
        if playing () then
            play_from time
        else
            if time >= 0.0f<ms> && time < duration () then
                Bass.ChannelSetPosition(
                    now_playing.ID,
                    Bass.ChannelSeconds2Bytes(now_playing.ID, float <| time / 1000.0f<ms>)
                )
                |> display_bass_error
            else if channel_playing then
                Bass.ChannelStop now_playing.ID |> display_bass_error
                channel_playing <- false

            timer_start <- time
            timer.Reset()
            update_time()

    let pause () =
        let time = time ()

        if time >= 0.0f<ms> && time < duration () then
            Bass.ChannelPause now_playing.ID |> display_bass_error

        timer.Stop()
        paused <- true

    let resume () =
        let time = time ()

        if time >= 0.0f<ms> && time < duration () then
            Bass.ChannelPlay now_playing.ID |> display_bass_error

        timer.Start()
        paused <- false

    let change_rate (new_rate) =
        let rate_changed = rate <> new_rate
        let time = time ()
        rate <- new_rate

        if enable_pitch_rates then
            Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Tempo, 0.0f) |> display_bass_error
            Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Frequency, float32 now_playing.Frequency * float32 rate) |> display_bass_error
        else
            Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Tempo, (rate - 1.0f<rate>) * 100.0f</rate>) |> display_bass_error
            Bass.ChannelSetAttribute(now_playing.ID, ChannelAttribute.Frequency, float32 now_playing.Frequency) |> display_bass_error

        if rate_changed then
            seek time

    let playback_rate () = rate

    let set_pitch_rates_enabled (enabled: bool) =
        enable_pitch_rates <- enabled
        change_rate rate

    let set_low_pass (amount: float32) =
        low_pass_target <- amount
        now_playing.SetLowPass low_pass_amount

    let set_local_offset (offset) = _local_offset <- offset
    let set_global_offset (offset) = _global_offset <- offset

    let private song_loader =
        { new Async.SwitchService<Time * Time * string option * SongLoadAction, Song * SongLoadAction>() with
            override this.Process((preview, last_note, path, after_load)) =
                async {
                    return
                        match path with
                        | Some p -> Song.FromFile (preview, last_note, p), after_load
                        | None -> Song.Default, after_load
                }

            override this.Handle((song, after_load: SongLoadAction)) =
                loading <- false
                now_playing <- song
                change_rate rate

                song.SetLowPass low_pass_amount

                match after_load with
                | SongLoadAction.PlayFromPreview ->
                    (if paused then seek else play_from) song.PreviewPoint
                    Bass.ChannelSetAttribute(song.ID, ChannelAttribute.Volume, 0.0f) |> display_bass_error
                    Bass.ChannelSlideAttribute(song.ID, ChannelAttribute.Volume, 1.0f, CROSSFADE_DURATION_MS) |> display_bass_error
                | SongLoadAction.PlayFromBeginning ->
                    (if paused then seek else play_from) 0.0f<ms>
                | SongLoadAction.Wait -> ()

                on_loaded_ev.Trigger()
        }

    let change (path: string option, offset: Time, new_rate: float32<rate>, (preview: Time, chart_last_note: Time), after_load: SongLoadAction) =
        let path_changed = path <> load_path
        load_path <- path
        set_local_offset offset
        change_rate new_rate

        if path_changed then

            let current = now_playing
            let slide_completed _ _ _ _ = current.Free()

            if current.ID <> 0 then
                Bass.ChannelSetSync(current.ID, SyncFlags.Slided ||| SyncFlags.Onetime, 0, SyncProcedure(slide_completed)) |> display_bass_error
                Bass.ChannelSetSync(current.ID, SyncFlags.End ||| SyncFlags.Onetime, 0, SyncProcedure(slide_completed)) |> display_bass_error
                Bass.ChannelSlideAttribute(current.ID, ChannelAttribute.Volume, 0.0f, CROSSFADE_DURATION_MS) |> display_bass_error

            channel_playing <- false
            loading <- true
            song_loader.Request(preview, chart_last_note, path, after_load)

    let update (elapsed_ms: float) =
        let time_last_update = _time
        update_time()

        if low_pass_target <> low_pass_amount then
            low_pass_amount <- lerp (float32 <| Math.Pow(0.994, elapsed_ms)) low_pass_target low_pass_amount
            if abs (low_pass_amount - low_pass_target) < 0.01f then low_pass_amount <- low_pass_target
            now_playing.SetLowPass low_pass_amount

        song_loader.Join()

        if playing () && _time >= 0.0f<ms> && _time < now_playing.Duration && not channel_playing then

            channel_playing <- true

            Bass.ChannelSetPosition(now_playing.ID, Bass.ChannelSeconds2Bytes(now_playing.ID, float <| _time / 1000.0f<ms>))
            |> display_bass_error

            Bass.ChannelPlay now_playing.ID |> display_bass_error

        elif _time > now_playing.Duration then

            match on_finish with
            | SongFinishAction.LoopFromPreview ->
                if _time >= now_playing.LastNote then
                    play_from now_playing.PreviewPoint
            | SongFinishAction.LoopFromBeginning ->
                if _time >= now_playing.LastNote then
                    play_from 0.0f<ms>
            | SongFinishAction.Wait -> ()
            | SongFinishAction.Custom action ->
                if channel_playing then
                    action ()

            channel_playing <- false

        elif channel_playing && playing () && timer.ElapsedMilliseconds > 80L && timer.ElapsedMilliseconds < 2000L then
            let audio_position = Bass.ChannelBytes2Seconds(now_playing.ID, Bass.ChannelGetPosition(now_playing.ID, PositionFlags.Bytes)) |> float32

            let clock_elapsed = _time - time_last_update

            let off_by = time_compensated () - audio_position * 1000.0f<ms>
            let adjustment = abs off_by |> min (clock_elapsed * 0.8f)

            if adjustment > 0.5f<ms> && abs off_by < 200.0f<ms> then
                seek_inaccuracy_compensation <- seek_inaccuracy_compensation + if off_by > 0.0f<ms> then adjustment else -adjustment