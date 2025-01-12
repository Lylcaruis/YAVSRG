﻿namespace Prelude.Data.Library

open System
open System.IO
open System.IO.Compression
open System.Text.RegularExpressions
open Percyqaz.Data
open Percyqaz.Common
open Prelude.Charts
open Prelude.Charts.Conversions

module Imports =

    let OSU_SONG_FOLDER =
        Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "osu!", "Songs")

    let STEPMANIA_PACK_FOLDER =
        Path.Combine(Path.GetPathRoot Environment.CurrentDirectory, "Games", "Stepmania 5", "Songs")

    let ETTERNA_PACK_FOLDER =
        Path.Combine(Path.GetPathRoot Environment.CurrentDirectory, "Games", "Etterna", "Songs")

    let QUAVER_SONG_FOLDER =
        Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.ProgramFilesX86, "Steam", "steamapps", "common", "Quaver", "Songs")

    [<Json.AutoCodec>]
    type MountedChartSourceType =
        | Pack of name: string
        | Library

    [<Json.AutoCodec>]
    type MountedChartSource =
        {
            SourceFolder: string
            mutable LastImported: DateTime
            Type: MountedChartSourceType
            ImportOnStartup: bool
        }
        static member Pack(name: string, path: string) =
            {
                SourceFolder = path
                LastImported = DateTime.UnixEpoch
                Type = Pack name
                ImportOnStartup = false
            }

        static member Library(path: string) =
            {
                SourceFolder = path
                LastImported = DateTime.UnixEpoch
                Type = Library
                ImportOnStartup = false
            }

    let private RATE_REGEX =
        Regex(
            """((^|\s)([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)($|\s))|(x([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?))|(([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)[x\]])"""
        )

    let detect_rate_mod (difficulty_name: string) : float32<rate> option =
        let m = RATE_REGEX.Match difficulty_name

        if m.Success then
            let r = m.Value.Trim([| ' '; 'x'; ']' |]).Replace(',', '.')

            match Single.TryParse r with
            | true, r -> Some (r * 1.0f<rate>)
            | false, _ -> None
        else
            None

    let filter_rates (path: string) (results: Result<ImportChart, string * string> list) : Result<ImportChart, string * string> list =
        results
        |> List.map (
            function
            | Ok import ->
                match detect_rate_mod import.Header.DiffName with
                | Some rate ->
                    let original =
                        results
                        |> List.tryPick (
                            function
                            | Ok { Chart = original; Header = header } ->
                                let original_duration = original.LastNote - original.FirstNote
                                let incoming_duration = import.Chart.LastNote - import.Chart.FirstNote
                                if
                                    original.Notes.Length = import.Chart.Notes.Length &&
                                    abs (incoming_duration * float32 rate - original_duration) < 5.0f<ms>
                                then
                                    match import.Header.Origins |> Set.toSeq |> Seq.tryHead with
                                    | Some (ChartOrigin.Osu (md5, set_id, map_id, _)) ->
                                        Some (header, ChartOrigin.Osu (md5, set_id, map_id, Some (rate, import.Chart.FirstNote)))
                                    | _ -> None
                                else None
                            | _ -> None
                        )
                    match original with
                    | Some (h, alt_rate_origin) ->
                        h.Origins <- h.Origins.Add alt_rate_origin
                        Error (path, sprintf "Skipping %.2fx rate of another map" rate)
                    | None -> Ok import
                | None -> Ok import
            | Error skipped_conversion -> Error skipped_conversion
        )

    let convert_song_folder =
        { new Async.Service<string * ConversionOptions * Library, ConversionResult>() with
            override this.Handle((path, config, { Charts = chart_db })) =
                async {
                    let results =
                        Directory.EnumerateFiles path
                        |> Seq.collect (
                            function
                            | ChartFile _ as file ->
                                let action = { Config = config; Source = file }
                                try
                                    convert_chart_file action
                                with err ->
                                    Logging.Error "Unhandled exception converting '%s': %O" file err
                                    []
                            | _ -> []
                        )
                        |> Seq.map (
                            function
                            | Ok import ->
                                match Chart.check import.Chart with
                                | Ok chart -> Ok { import with Chart = chart }
                                | Error reason ->
                                    Logging.Error "Conversion produced corrupt chart (%s): %s" path reason
                                    Error (path, sprintf "Corrupt (%s)" reason)
                            | Error skipped_conversion -> Error skipped_conversion
                        )
                        |> List.ofSeq

                    let filtered = filter_rates path results

                    let mutable success_count = 0
                    let charts = filtered |> List.choose (function Ok c -> success_count <- success_count + 1; Some c | _ -> None)
                    ChartDatabase.import charts chart_db
                    return {
                        ConvertedCharts = success_count
                        SkippedCharts = filtered |> List.choose (function Error skipped -> Some skipped | _ -> None)
                    }
                }
        }

    let convert_pack_folder =
        { new Async.Service<string * ConversionOptions * Library, ConversionResult>() with
            override this.Handle((path, config, library)) =
                async {
                    let mutable results = ConversionResult.Empty
                    for song_folder in
                        (Directory.EnumerateDirectories path
                         |> match config.ChangedAfter with
                            | None -> id
                            | Some timestamp -> Seq.filter (fun path -> Directory.GetLastWriteTime path >= timestamp)) do
                        let! result = convert_song_folder.RequestAsync(song_folder, config, library)
                        results <- ConversionResult.Combine result results
                    return results
                }
        }

    let convert_folder_of_oszs =
        { new Async.Service<string * Library, ConversionResult>() with
            override this.Handle((folder_of_oszs, library)) =
                async {
                    let config =
                        { ConversionOptions.Default with
                            MoveAssets = true
                            PackName = Path.GetFileName folder_of_oszs
                        }
                    let mutable results = ConversionResult.Empty
                    for osz in Directory.EnumerateFiles folder_of_oszs do
                        match osz with
                        | ChartArchive ".osz" ->
                            let extracted_folder = Path.ChangeExtension(osz, null).TrimEnd(' ', '.')
                            if
                                Directory.Exists(extracted_folder)
                                && Directory.EnumerateFileSystemEntries(extracted_folder) |> Seq.isEmpty |> not
                            then
                                Logging.Error "Can't extract osz to %s because that folder exists already" extracted_folder
                                results <- { results with SkippedCharts = (osz, "Extraction failed") :: results.SkippedCharts }
                            else
                                try
                                    ZipFile.ExtractToDirectory(osz, extracted_folder)
                                    let! result = convert_song_folder.RequestAsync(extracted_folder, config, library)
                                    Directory.Delete(extracted_folder, true)
                                    results <- ConversionResult.Combine result results
                                with err ->
                                    Logging.Error "Unexpected error in '%s': %O" extracted_folder err
                                    results <- { results with SkippedCharts = (extracted_folder, sprintf "Unexpected error: %s" err.Message) :: results.SkippedCharts }
                        | _ -> ()
                    return results
                }
        }

    let import_mounted_source =
        { new Async.Service<MountedChartSource * Library, ConversionResult>() with
            override this.Handle((source, library)) =
                async {
                    match source.Type with
                    | Pack packname ->
                        let config =
                            { ConversionOptions.Default with
                                MoveAssets = false
                                ChangedAfter = Some source.LastImported
                                PackName = packname
                            }

                        let! result = convert_pack_folder.RequestAsync(source.SourceFolder, config, library)
                        source.LastImported <- DateTime.UtcNow
                        return result
                    | Library ->
                        let mutable results = ConversionResult.Empty
                        for pack_folder in
                            Directory.EnumerateDirectories source.SourceFolder
                            |> Seq.filter (fun path -> Directory.GetLastWriteTime path >= source.LastImported) do
                            let! result =
                                convert_pack_folder.RequestAsync(
                                    pack_folder,
                                    { ConversionOptions.Default with
                                        MoveAssets = false
                                        ChangedAfter = Some source.LastImported
                                        PackName = Path.GetFileName pack_folder
                                    },
                                    library
                                )
                            results <- ConversionResult.Combine result results

                        source.LastImported <- DateTime.UtcNow
                        return results
                }
        }

    let convert_stepmania_pack_zip =
        { new Async.Service<string * string * Library, ConversionResult option>() with
            override this.Handle((path, pack_name, library)) =
                async {
                    let dir = Path.ChangeExtension(path, null).TrimEnd(' ', '.')

                    if
                        Directory.Exists(dir)
                        && Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty |> not
                    then
                        Logging.Error "Can't extract zip to %s because that folder exists already" dir
                        return None
                    else
                        ZipFile.ExtractToDirectory(path, dir)

                        match dir with
                        | FolderOfPacks ->
                            let mutable results = ConversionResult.Empty
                            for pack_folder in Directory.EnumerateDirectories dir do
                                let! result =
                                    convert_pack_folder.RequestAsync(
                                        pack_folder,
                                        { ConversionOptions.Default with
                                            EtternaPackName = Some pack_name
                                            PackName = Path.GetFileName pack_folder
                                            MoveAssets = true
                                        },
                                        library
                                    )
                                results <- ConversionResult.Combine result results

                            Directory.Delete(dir, true)
                            return Some results
                        | _ ->
                            Logging.Warn "'%s': Extracted zip does not match the usual structure for a StepMania pack" dir
                            Directory.Delete(dir, true)
                            return None
                }
        }

    let private log_skipped (result: ConversionResult) =
        let skipped = result.SkippedCharts.Length
        if skipped > 0 then
            let dump =
                result.SkippedCharts
                |> Seq.map (fun (path, reason) -> sprintf "%s -> %s" path reason)
                |> String.concat "\n "
            Logging.Info "Successful import of %i file(s) also skipped %i file(s):\n %s" result.ConvertedCharts skipped dump

    let rec private auto_detect_import (path: string, move_assets: bool, library: Library) : Async<ConversionResult option> =
        async {
            match File.GetAttributes path &&& FileAttributes.Directory |> int with
            | 0 ->
                match path with
                | ChartFile ext ->
                    let! result =
                        convert_song_folder.RequestAsync(
                            Path.GetDirectoryName path,
                            { ConversionOptions.Default with
                                PackName =
                                    if ext = ".osu" then "osu!"
                                    elif ext = ".qua" then "Quaver"
                                    else "Singles"
                            },
                            library
                        )
                    log_skipped result
                    return Some result
                | ChartArchive ext ->
                    let dir = Path.ChangeExtension(path, null).TrimEnd(' ', '.')

                    if
                        Directory.Exists(dir)
                        && Directory.EnumerateFileSystemEntries(dir) |> Seq.isEmpty |> not
                    then
                        Logging.Error "Can't extract %s to %s because that folder exists already" ext dir
                        return None
                    else
                        ZipFile.ExtractToDirectory(path, dir)
                        let! result = auto_detect_import (dir, true, library)
                        Directory.Delete(dir, true)
                        return result
                | _ ->
                    Logging.Warn "%s: Unrecognised file for import" path
                    return None
            | _ ->
                match path with
                | SongFolder ext ->
                    let! result =
                        convert_song_folder.RequestAsync(
                            path,
                            { ConversionOptions.Default with
                                MoveAssets = move_assets
                                PackName =
                                    if ext = ".osu" then "osu!"
                                    elif ext = ".qua" then "Quaver"
                                    else "Singles"
                            },
                            library
                        )
                    log_skipped result
                    return Some result
                | FolderOfOszs ->
                    let! result =
                        convert_folder_of_oszs.RequestAsync(
                            path,
                            library
                        )
                    log_skipped result
                    return Some result
                | PackFolder ->
                    let packname =
                        match Path.GetFileName path with
                        | "Songs" ->
                            if path |> Path.GetDirectoryName |> Path.GetFileName = "osu!" then
                                "osu!"
                            elif path |> Path.GetDirectoryName |> Path.GetFileName = "Quaver" then
                                "Quaver"
                            else
                                "Songs"
                        | s -> s

                    let! result =
                        convert_pack_folder.RequestAsync(
                            path,
                            { ConversionOptions.Default with
                                PackName = packname
                                MoveAssets = move_assets
                            },
                            library
                        )
                    log_skipped result
                    return Some result
                | FolderOfPacks ->
                    let mutable results = ConversionResult.Empty
                    for pack_folder in Directory.EnumerateDirectories path do
                        let! result =
                            convert_pack_folder.RequestAsync(
                                pack_folder,
                                { ConversionOptions.Default with
                                    PackName = Path.GetFileName pack_folder
                                    MoveAssets = move_assets
                                },
                                library
                            )
                        results <- ConversionResult.Combine result results

                    log_skipped results
                    return Some results
                | _ ->
                    Logging.Warn "%s: No importable folder structure detected" path
                    return None
        }

    let auto_convert =
        { new Async.Service<string * bool * Library, ConversionResult option>() with
            override this.Handle((path, move_assets, library)) =
                auto_detect_import (path, move_assets, library)
        }