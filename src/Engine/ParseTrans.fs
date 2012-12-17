(*
 * COBOL Type Inference
 * ParseTrans.fs: parse-time translators
 * (C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
#light

module CobolAnalyzer.Engine.ParseTrans

open System
open Prelude
open Absyn
open Absyn.Ideal
open Log

(* parse-time utilities *)

let mutable stmt_count = 0;

let mutable keyword_token_mode = false
let mutable clear_buffer_at_eol = true

(* parse error context untyped exception for working around yacc exception boxing *)
exception ParseErrorContext of obj


(* environment of fresh vars generated while parsing and to be added to the global var env *)
let procdiv_bindings : Collections.Generic.ICollection<_> = new Collections.Generic.LinkedList<Ty.binding> () :> Collections.Generic.ICollection<_>
    

(* fresh stuff generators *)   

let generate_fresh_id prefix = sprintf "$__%s__%d" prefix (fresh ())

let generate_fresh_label () = generate_fresh_id "label"

let generate_fresh_var () = 
    let fresh_id = generate_fresh_id "var"
    let fresh_binding = (fresh_id, Ty.Num (true, Config.Trans.fresh_var_numformat_size, 0us, Ty.BCD), None)
    procdiv_bindings.Add fresh_binding;
    fresh_id


(* annotated constructs shortcuts *)

let private generate_annot x cmd = annot<_>.create x (generate_fresh_id (sprintf "%s_%O" cmd x))

let Goto id = GotoAnnot (generate_annot id "goto")
let Perform parid = PerformAnnot (generate_annot parid "perform")
let Id x = IdAnnot (generate_annot x "id")


(* program-location utility *)

let locate (parseState : Microsoft.FSharp.Text.Parsing.IParseState) n x =
    let (spos, epos) = parseState.InputRange n
    in
        Loc (x, (spos.Line, epos.Line))
    

(* statement lexer stateful token buffer *)

type token_buffer () =
    let toks = new Collections.Generic.List<obj * string> ()
with
    member self.append tok lexbuf =
        ignore <| toks.Add ((tok, (Lexing.LexBuffer<_>.LexemeString lexbuf).Trim [|' '; '\t'; '\n'; '\r'|]))

    member self.consume_as_special_statement extract_string cons =
        let kwdo = ref None
        let (s, ids) =
            let append_id tok ids =
                match extract_string tok with
                    Some (s, false) -> s :: ids
                  | Some (s, true)  -> (if Option.isNone !kwdo then kwdo := Some s); ids
                  | None            -> ids
            in
                Seq.fold (fun (s, ids) (tok, lexeme) -> (s + " " + lexeme, append_id tok ids)) ("", []) toks
        in
            cons (s.Trim (), !kwdo, List.rev ids)
        
    member self.clear = toks.Clear ()

let statement_parser_token_buffer = new token_buffer ()



(* block statements manipulators *)

let block_of_statements sts =
  match sts with
    []      -> unexpected "block_of_statements: empty block"
  | [stmt]  -> stmt 
  | (Loc (stmt0, (sl0, _)) :: stmts) as all ->
        match List.nth stmts (List.length stmts - 1) with
            Loc (_, (_, el1)) -> Loc (Block (None, all), (sl0, el1))
    

(* recursively unfold anonymous blocks within statements *)
let rec unfold_blocks_in_statement (Loc (stmt', loc) as stmt) =
    match stmt' with
        If (e, tst, esto) ->
            let f st = st |> unfold_blocks_in_statement |> block_of_statements
            in
                [Loc (If (e, f tst, Option.map f esto), loc)]

      | Block (None, stmts) -> unfold_blocks_in_statements stmts
      | Block (lbo, stmts)  -> [Loc (Block (lbo, unfold_blocks_in_statements stmts), loc)]
      | Nop                -> []
      | _                   -> [stmt]

and unfold_blocks_in_statements stmts = List.concat (List.map unfold_blocks_in_statement stmts)


(* goto_stmt *)
let goto_depending_to_if parseState (identifiers, cur_idx) variable = 
    let locate = locate parseState 1
    let rec generate_if identifiers cur_idx variable =
        let condition index variable = BinOp (LValue (Id variable), RelBinOp Eq, Lit (Int index))
        match identifiers with
            []        -> None
          | id :: ids ->
                let b_then = locate <| Block (None, [locate <| Goto id])
                in
                    Some (If (condition cur_idx variable, b_then,
                                match generate_if ids (cur_idx + 1L) variable with 
                                    None      -> None
                                  | Some stmt -> Some (locate (Block (None, [locate stmt])))))
    in
        match generate_if identifiers cur_idx variable with
            None      -> raise (Unexpected "empty identifier list in GOTO statement") 
          | Some stmt -> stmt


(* perform statement - all formats *)
let loop_target_to_stmt  = function
    | (Some label_pair, None)  -> Perform label_pair
    | (None, Some stmtl)       -> Block (None, stmtl)
    | _                        -> raise (Unexpected "perform_until_varying_stmt with incorrect parameters")
    
let perform_times_to_loop parseState target times_expr =
    let locate = locate parseState 1
    let loop_body = loop_target_to_stmt target
    let counter_id = generate_fresh_var ()
    let loop_start_label = generate_fresh_label ()
    let if_body = Block (None, [locate <| loop_body;
                                locate <| Assign (Id counter_id, BinOp (LValue (Id counter_id), ArithBinOp Minus, Lit (Int 1L)));
                                locate <| Goto loop_start_label])
    
    let loop_block = Block (Some loop_start_label,
                                [locate <| If (BinOp (LValue (Id counter_id), RelBinOp Gt, Lit (Int 0L)), locate if_body, None)])
    in
        Block (None, [locate <| Assign (Id counter_id, times_expr); locate <| loop_block])


let perform_until_to_loop parseState target condition test_is_after =
    let locate = locate parseState 1
    let loop_body = locate <| loop_target_to_stmt target
    let loop_start_label = generate_fresh_label ()
    
    match test_is_after with
        false -> 
            let if_body = Block (None, [loop_body; locate <| Goto loop_start_label])
            in
                Block (Some loop_start_label, [locate <| If (UnOp (LogicUnOp Not, condition), locate <| if_body, None)])

      | true ->
            Block (Some loop_start_label, [loop_body; locate <| If (UnOp (LogicUnOp Not, condition), locate <| Goto loop_start_label, None)])
            
    
let perform_until_varying_to_loop parseState target (counter_id, start_val, incr_val, condition) test_is_after =
    let locate = locate parseState 1
    let loop_start_label = generate_fresh_label ()
    let loop_body = locate <| loop_target_to_stmt target

    let incr_expr = BinOp (LValue (Id counter_id), ArithBinOp Plus, incr_val)
    let assign_counter_st e = locate <| Assign (Id counter_id, e)
    let incr_block_footer = [assign_counter_st incr_expr; locate <| Goto loop_start_label]
    let if_st if_body = locate <| If (UnOp (LogicUnOp Not, condition), if_body, None)
    let incr_outer_block loop_block = Block (None, [assign_counter_st start_val; loop_block])
    in
        match test_is_after with
            false -> 
                let if_body = locate <| Block (None, loop_body :: incr_block_footer)
                let loop_block = locate <| Block (Some loop_start_label, [if_st if_body])
                in
                    incr_outer_block loop_block
          | true ->
                let if_body = locate <| Block (None, incr_block_footer)
                let loop_block = locate <| Block (Some loop_start_label, [loop_body; if_st if_body])
                in
                    incr_outer_block loop_block


let perform_until_varying_after_to_loop parseState target nested_varying_loops test_is_after =
    let locate = locate parseState 1
    let rec nested_varying_loops_to_block target = function
             [(counter_id, start_val, incr_val, condition)] -> 
                perform_until_varying_to_loop parseState target (counter_id, start_val, incr_val, condition) test_is_after
            
           | (counter_id, start_val, incr_val, condition) :: nested_loops ->
                let cur_target = nested_varying_loops_to_block target nested_loops
                in perform_until_varying_to_loop parseState (None, Some [locate cur_target]) (counter_id, start_val, incr_val, condition) test_is_after

           | _  -> raise (Unexpected "perform_until_varying_after_stmt with incorrect nested loop parameters")
    in 
        nested_varying_loops_to_block target nested_varying_loops


(* evaluate_stmt *)

let evaluate_to_if parseState expr (whens, whenother_statements) =
    let locate = locate parseState 1
    let ifst when_expr stmts elseo = If (BinOp (expr, RelBinOp Eq, when_expr), locate <| Block (None, stmts), elseo)
    in
        List.foldBack (fun (e, sts) st -> ifst e sts (Some (locate st))) whens (Block (None, whenother_statements))
   

(* simple statements parse-time translators *)

(* arith_stmt *)    
let arith_stmt_trans parseState operand_2 operation other_operands =
    let locate = locate parseState 1
    let operands_to_stmt = fun (operand_1, res) -> Assign (res, BinOp (operand_1, operation, operand_2))
    match other_operands with
        []             -> raise (Unexpected "empty arithmetic statement encountered")
      | [operands]     -> operands_to_stmt operands
      | _              -> Block (None, (other_operands |> List.map (locate << operands_to_stmt)))

(* move_stmt *)
let move_stmt_trans parseState rvalue lvalues =
    let locate = locate parseState 1
    let lvalues_to_stmt = fun lv -> Assign (lv, rvalue)
    match lvalues with
        []             -> raise (Unexpected "empty arithmetic statement encountered")
      | [id]           -> lvalues_to_stmt id
      | _              -> Block (None, (lvalues |> List.map (locate << lvalues_to_stmt)))

(* condition unfolding *)
let rec unfold_condition left_expr binop = function
      [(item, None)]        -> BinOp (left_expr, binop, item)
    | (item, Some op) :: xs -> BinOp (BinOp (left_expr, binop, item), op, unfold_condition left_expr binop xs)
    | _                     -> raise (Unexpected "invalid condition encountered")

