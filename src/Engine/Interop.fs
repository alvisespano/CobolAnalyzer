(*
 * COBOL Type Inference
 * Interop.fs: interoperability API with C#
 * (C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
#light

module CobolAnalyzer.Engine.Interop

open Prelude

module Application =

    type Mode = UI | CmdLine

    type ConsoleOwner = Parent | Self

    let mutable mode = UI
    let mutable consoleOwner = Parent


module Cobol =

    open Absyn

    type Program (filename) =
        let prg = Parsing.Cobol.load_and_parse_program filename
        let prg' = PostTrans.cobol_program_to_ideal prg
        let mutable envso = None

        member self.Cobol with get () = prg
        member self.Ideal with get () = prg'
        member self.Environments with get () = envso

        member self.Typecheck () = envso <- Some (Typing.typecheck_program self.Ideal)
        
        member self.PrettyIdeal () = Ideal.pretty_program Absyn.Ideal.pretty_annots_invisible self.Ideal

        member self.PrettyIdealAnnotated () =
            Option.map (fun (envs : Typing.environments) ->
                Ideal.pretty_program
                    { Ideal.pretty_annots_invisible with
                        id = function Ideal.Annot (x, uid) ->
                                        let t = match Typing.Env.search uid envs.topo with
                                                    None    -> sprintf "? [%s]" uid
                                                  | Some ft -> ft.pretty
                                        in
                                            sprintf "(%s : %s)" x t }
                    prg')
                envso

                