(*
 * COBOL Type Inference
 * Prelude.fs: misc utils
 * (C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
#light

module CobolAnalyzer.Engine.Log

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open System.IO
open Prelude
open Printf


module private Config =
    let datetime_max_len = 22
    let priority_max_len = 4


(* fast color printer: ugly, imperative, lambda-unfolded *)

let private prompter =
    let fw =
        let fstr = new FileStream (Config.Log.filename, FileMode.Create, FileAccess.Write)
        in
            new StreamWriter (fstr)
    let tablen = ref 0
    let out (s : string)=
        Console.Write s;
        fw.Write s;
        tablen := !tablen + s.Length;
    let outcol fg bg (s : string)=
        Console.ForegroundColor <- fg;
        Console.BackgroundColor <- bg;
        out s
    let outsq fg bg len s =
        outcol ConsoleColor.White ConsoleColor.Black "[";
        outcol fg bg s;
        outcol ConsoleColor.White ConsoleColor.Black "]";
        let dlen = len - (s.Length + 2)
        if dlen > 0 then Console.ResetColor (); out (new String (' ', dlen))
    let outsqfg fg len s = outsq fg ConsoleColor.Black len s
    let pad n = Console.ResetColor (); out (new String (' ', n))
    in
        fun (hdo : string option) (col : ConsoleColor) ->
            let colenumty = col.GetType ()
            let darkcol = ConsoleColor.Parse (colenumty, "dark" + ConsoleColor.GetName (colenumty, col), true) :?> ConsoleColor
            let col = col
            in
                fun marks lvo (s : string) ->
                    tablen := 0;
                    // datetime              
                    (if Config.Log.show_datetime then outsqfg Config.Log.Colors.datetime Config.datetime_max_len (sprintf "%A" DateTime.Now));
                    // header
                    (match hdo with
                        Some hd -> outsq col darkcol 14 hd
                      | None    -> pad 14);
                    // priority
                    (if Config.Log.show_priority then
                        match lvo with
                            Some (lv : pri) -> outsqfg Config.Log.Colors.priority Config.priority_max_len lv.pretty
                          | None            -> pad 4);
                    // urgency
                    (if Config.Log.show_urgency then outcol Config.Log.Colors.urgency ConsoleColor.Black (new String ('!', marks) + new String (' ', 5 - marks)));
                    // body
                    let at = Console.CursorLeft
                    let (bodyfgcol, bodybgcol) =
                        if Config.Log.shade_urgency then
                            match marks with
                                  0 
                                | 1 -> darkcol, ConsoleColor.Black
                                | 2 -> col, ConsoleColor.Black
                                | 3 -> ConsoleColor.White, darkcol
                                | 4 -> darkcol, ConsoleColor.White
                                | _ -> unexpected "marks = %d" marks
                        else (col, ConsoleColor.Black)
                    let outbody (s : string) =
                        Console.CursorLeft <- at;
                        Console.ForegroundColor <- bodyfgcol;
                        Console.BackgroundColor <- bodybgcol;
                        Console.Write s;
                        Console.ResetColor ();
                        Console.WriteLine "";
                    in
                        let sa = s.Split [|'\n'|] |> Array.map (split_string_on_size (Console.BufferWidth - at - 1 - Config.Log.multiline_tab_width))
                        let tab = new String (' ', Config.Log.multiline_tab_width)
                        Array.iter (Array.iteri (fun i s -> outbody ((if i > 0 then tab else "") + s))) sa;
                        fw.WriteLine s
                   

let private prompt_printf prompt marks lvo fmt = kprintf (prompt marks lvo) fmt

let private null_printf _ _ = kprintf (fun _ -> ())

let private print prompt thre lv fmt =
    (if lv >= thre then prompt_printf prompt (int lv - int thre) else null_printf 0) (Some lv) fmt

type 'a logger = pri -> StringFormat<'a, unit> -> 'a

let private debug_prompt = prompter (Some "DEBUG") Config.Log.Colors.debug
let debug lv fmt = print debug_prompt Config.Log.debug_threshold lv fmt

let private hint_prompt = prompter (Some "HINT") Config.Log.Colors.hint
let hint lv fmt = print hint_prompt Config.Log.hint_threshold lv fmt

let private warn_prompt = prompter (Some "WARNING") Config.Log.Colors.warn
let warn lv fmt = print warn_prompt Config.Log.warn_threshold lv fmt

let private msg_prompt = prompter None Config.Log.Colors.msg
let msg lv fmt = print msg_prompt Config.Log.msg_threshold lv fmt

let private error s = prompt_printf (prompter (Some s) Config.Log.Colors.error) 0 None
let recoverable_error fmt = error "RECOVERABLE" fmt
let fatal_error fmt = error "FATAL ERROR" fmt
let unexpected_error fmt = error "UNEXPECTED ERROR" fmt

let line_feed () = Console.WriteLine ""
