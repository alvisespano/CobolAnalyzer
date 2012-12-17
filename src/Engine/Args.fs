(*
 * COBOL Type Inference
 * Ver.fs: version information
 * (C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
#light

module CobolAnalyzer.Engine.Args

open System
open Prelude
open Log

let mutable filename = ""
let mutable ideal = false
let mutable ui = false

let private usage =
    sprintf "\nCobolInference v%d.%d.%d\n(C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia\n\nusage: %s <FILENAME>" Ver.major Ver.minor Ver.revision Config.exe_name

let private others s = filename <- s

module private Entry =
    let private arg argty (name : string) help defopt =
        ArgInfo ((if name.Length = 1 then sprintf "-%s" else sprintf "--%s") name,
                 argty,
                 match defopt with None -> help | Some def -> sprintf "%s (default = %O)" help def)

    let int name f help def = [|arg (ArgType.Int f) name help def|]
    
    let flag name f help = [|arg (ArgType.Unit f) name help None|]

    let string name f help def = [|arg (ArgType.String f) name help def|]

    let bool name f help =
        let noname = sprintf "no-%s" name
        in
            [|arg (ArgType.Unit (fun () -> f true)) name help None;
              arg (ArgType.Unit (fun () -> f false)) noname help None|]


let private infos = [|
    Entry.flag "pedantic" (fun () -> Config.Log.set_all_thresholds Min) "set all log thresholds to minimum level";
    Entry.flag "strict" (fun () -> Config.Typing.tolerant <- false) "type analyzer fails on errors";
    Entry.flag "v" (fun () -> Config.Log.set_all_thresholds Low) "set all log thresholds to low level";
    Entry.flag "ideal" (fun () -> ideal <- true) "force input file to be treated as IL source code";
    Entry.flag "quiet" (fun () -> Config.Log.set_all_thresholds High) "set all log thresholds to high level";
    Entry.flag "ignore-newline" (fun () -> Config.Parsing.newline_is_separator <- false) "do not treat newline as a statement separator in parsing";
    Entry.flag "print-typed-src" (fun () -> Config.print_typed_source <- true) "pretty print source with type annotations";
    Entry.bool "show-log-priority" (fun b -> Config.Log.show_priority <- b) "show log lines priority";
    Entry.bool "show-log-urgency" (fun b -> Config.Log.show_urgency <- b) "show log lines urgency";
    Entry.bool "show-log-datetime" (fun b -> Config.Log.show_datetime <- b) "show datetime in log lines";
    Entry.string "log-file" (fun s -> Config.Log.filename <- s) "set log filename" (Some Config.Log.filename);
    Entry.string "debug-threshold" (fun s -> Config.Log.debug_threshold <- pri.Parse s) "set debug verbosity threshold" (Some Config.Log.debug_threshold);
    Entry.string "msg-threshold" (fun s -> Config.Log.msg_threshold <- pri.Parse s) "set informational messages verbosity threshold" (Some Config.Log.msg_threshold);
    Entry.string "hint-threshold" (fun s -> Config.Log.hint_threshold <- pri.Parse s) "set hint messages verbosity threshold" (Some Config.Log.hint_threshold);
    Entry.string "warn-threshold" (fun s -> Config.Log.warn_threshold <- pri.Parse s) "set warnings verbosity threshold" (Some Config.Log.warn_threshold);
  |]

let parse () =
    ArgParser.Parse (Array.concat infos, otherArgs = others, usageText = usage)

