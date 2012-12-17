(*
 * COBOL Type Inference
 * Main.fs: main code
 * (C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
#light

module CobolAnalyzer.Engine.Main

open System
open Microsoft.FSharp.Text
open Absyn
open Prelude
open Log
open System.IO

#if TEST
let main () =
    printf "test mode\n";
    0

#else

module CmdLineMode =

    type result =
      { filename : string
        program  : Ideal.program
        envs     : Typing.environments }


    module Ideal =

        let load_and_parse_program = Parsing.Ideal.load_and_parse_program

        let typecheck_program prog =
            msg Unmaskerable "%s" "typechecking program...";
            let r = Typing.typecheck_program prog
            msg Normal "%s" "typechecking done.";
            r

        let load_and_typecheck filename =
            let prog = load_and_parse_program filename
            let envs = typecheck_program prog
            in
              { filename = filename;
                program  = prog;
                envs     = envs }


    module Cobol =        

        let private load_and_parse_program filename =
            let prog = Parsing.Cobol.load_and_parse_program filename
            let prog' = PostTrans.cobol_program_to_ideal prog
            msg Normal "IL program:\n%s" (Ideal.pretty_program Ideal.pretty_annots_annotated prog');
            line_feed ();
            prog'
     
        let load_and_typecheck filename =
            let prog = load_and_parse_program filename
            let envs = Ideal.typecheck_program prog
            in
              { filename = filename;
                program  = prog;
                envs     = envs }


    let pretty_program_typed (r : result) =
        Ideal.pretty_program
            { Ideal.pretty_annots_invisible with
                id = function Ideal.Annot (x, uid) ->
                                let t = match Typing.Env.search uid r.envs.topo with
                                            None    -> sprintf "? [%s]" uid
                                          | Some ft -> ft.pretty
                                in
                                    sprintf "(%s : %s)" x t }
            r.program

    let main () =   
        if (Path.GetExtension Args.filename).ToLower () = ".il" then Args.ideal <- true;
        let r = (if Args.ideal then Ideal.load_and_typecheck else Cobol.load_and_typecheck) Args.filename
        if Config.print_typed_source then msg High "typed program:\n%s" (pretty_program_typed r);
        0


let unrecoverable_error header s = fatal_error "%s error: %s" header s; 1

let main () =   
    try
        Args.parse ();

        if Args.filename <> "" then
            Interop.Application.mode <- Interop.Application.CmdLine;
            msg Normal "application mode: %A" Interop.Application.mode;
            CmdLineMode.main ()
        else 0

    with :? Parsing.syntax_error as e           -> unrecoverable_error "parse" e.Message
       | :? PostTrans.translation_error as e    -> unrecoverable_error "translation" e.Message
       | :? Typing.type_error as e              -> unrecoverable_error "type" e.Message
       | Unexpected s                           -> unexpected_error "%s" s; 1
       | e                                      -> unexpected_error "uncaught exception %A: %s\n%s" (e.GetType ()) e.Message e.StackTrace; 1

#endif