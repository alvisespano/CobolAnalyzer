(*
 * COBOL Type Inference
 * PostTrans.fs: post-parsing translations
 * (C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
#light

module CobolAnalyzer.Engine.PostTrans

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Text
open Absyn
open Prelude
open Log
open ParseTrans

type translation_error (message, loc) =
    inherit located_error (message, loc)



let env_bindings_of_datadiv =
    let ty_of_fmt_string fmts qual =
        let ty_of_pic =
            let fmt_len l = FormatSize.of_int (List.length l)
            in function
            Cobol.DataDiv.AlphaFormat chs ->
                (if List.forall (fun ch -> ch = Cobol.DataDiv.A) chs then Ideal.Ty.Alpha else Ideal.Ty.AlphaNum) (fmt_len chs)

          | Cobol.DataDiv.NumFormat (sgn, csi, csd) ->
                let n = fmt_len csi
                let d = fmt_len csd
                let q =
                    match qual with
                        None                                                     -> Ideal.Ty.OnePerByte
                      | Some (Cobol.DataDiv.Comp | Cobol.DataDiv.Binary)         -> Ideal.Ty.Int (Ideal.Ty.word_len_of_num_format (n, d))
                      | Some Cobol.DataDiv.Comp1                                 -> Ideal.Ty.Float 32us
                      | Some Cobol.DataDiv.Comp2                                 -> Ideal.Ty.Float 64us
                      | Some (Cobol.DataDiv.Comp3 | Cobol.DataDiv.PackedDecimal) -> Ideal.Ty.BCD
                in
                    Ideal.Ty.Num (sgn, n, d, q)

        let fmt = Parsing.Cobol.parse_pic_format (Lexing.LexBuffer<_>.FromString fmts)
        in
            ty_of_pic fmt

    let arrayize ty = function 1u -> ty | n -> Ideal.Ty.Array (ty, n)

    let rec const_value_of_value =
        let conv_lit = function
            Cobol.Int n    -> Ideal.Int n
          | Cobol.Float x  -> Ideal.Float x
          | Cobol.String s -> Ideal.String s
        in function
            Cobol.DataDiv.Seq lits -> Ideal.Seq (List.map conv_lit lits)
          | Cobol.DataDiv.Thru lls -> raise (NotImplementedException "THRU")

    let rec f = function
        [] -> []
 
      | Loc (Cobol.DataDiv.Line (_, id, Cobol.DataDiv.Record, occurs), _) :: ((Loc (Cobol.DataDiv.Line (tab1, _, _, _), _) :: _) as lines) ->
            let (reclines, nextlines) = 
                let rec subfilter z = function
                    [] -> (z, [])
                  | (Unloc (Cobol.DataDiv.Line (tab, _, _, _)) as l :: ls) as all -> if tab >= tab1 then subfilter (z @ [l]) ls else (z, all)
                in
                    subfilter [] lines
            let rbs = f reclines
            let ty = Ideal.Ty.Record rbs
            let b = (id, arrayize ty occurs, None)
            in
                b :: f nextlines

      | Loc (Cobol.DataDiv.Line (_, id, Cobol.DataDiv.Picture (fmts, q, vo), occurs), loc) :: lines ->
            let ty = try ty_of_fmt_string fmts q
                     with :? Parsing.parse_error -> raise (translation_error (sprintf "variable '%s' has invalid picture format '%s'" id fmts, loc))
            let vo = Option.map const_value_of_value vo
            let b = (id, arrayize ty occurs, vo)
            in
                b :: f lines

      | [Loc (Cobol.DataDiv.Line (_, id, Cobol.DataDiv.Record, _), loc)] -> raise (translation_error (sprintf "ill-formed record '%s' has no fields" id, loc))
    in
        f


(* lvalue translator *)

open Ideal

let rec replace_ids_with_lvalues_in_statement (bs : Ty.bindings) =

    let rec in_statement st = replace_ids_with_lvalues_in_statement bs st

    and in_statement' = function
        Assign (lv, e)    -> Assign (in_lvalue lv, in_expr e)
      | If (e, st1, st2o) -> If (in_expr e, in_statement st1, Option.map in_statement st2o)
      | Call (id, acs)    -> Call (id, List.map in_actual acs)
      | Block (lbo, sts)  -> Block (lbo, List.map in_statement sts)
      | st                -> st  

    and in_expr = function
        BinOp (e1, op, e2)  -> BinOp (in_expr e1, op, in_expr e2)
      | UnOp (op, e)        -> UnOp (op, in_expr e)
      | LValue lv           -> LValue (in_lvalue lv)
      | e                   -> e

    and in_actual = function
        ByRef lv -> ByRef (in_lvalue lv)
      | ac       -> ac

    and in_lvalue = function
        Subscript (lv, e)   -> Subscript (in_lvalue lv, in_expr e)
      | Select (lv, lb)     -> Select (in_lvalue lv, lb)

      | IdAnnot ann ->
            let x = ann.value
            let ids =
                let rec f = function
                    (x', _, _) when x' = x -> Some [x]

                  | (x', Ty.Record bs', _) ->
                        match List.tryPick f bs' with
                            Some ids -> Some (x' :: ids)
                          | None     -> None

                  | _ -> None
                in
                    match List.tryPick f bs with
                        None     -> [x]
                      | Some ids -> ids
            in
                List.fold (fun lv id -> Select (lv, id)) (Id (List.head ids)) (List.tail ids)

    in
        function Loc (st, loc) -> Loc (in_statement' st, loc)



(* whole progam translator *)

let cobol_program_to_ideal (prg : Cobol.program) =
    let envbs = env_bindings_of_datadiv prg.data.value @ (List.ofSeq ParseTrans.procdiv_bindings)
    in
        { Ideal.procs = []
          Ideal.main = (prg.ideal_statement |> replace_ids_with_lvalues_in_statement envbs, envbs) }
