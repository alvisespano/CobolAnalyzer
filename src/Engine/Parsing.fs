(*
 * COBOL Type Inference
 * Parsing.fs: parsing utilities & custom parsers
 * (C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
#light

module CobolAnalyzer.Engine.Parsing

open System
open Prelude
open Log
open Absyn

(* parsing exceptions hierarchy *)

type syntax_error (message, loc, columns) =
    inherit located_error (message, loc)

    static member internal locate_from_lexbuf (lexbuf : Lexing.LexBuffer<_>) =
        let poss = lexbuf.StartPos
        let pose = lexbuf.EndPos
        let linee = pose.Line + 1
        let lines = poss.Line + 1
        let cole = pose.Column + 1
        let cols = poss.Column + 1
        in
            (lines, linee), (cols, cole)

    new (message, lexbuf : Lexing.LexBuffer<_>) =
        let (loc, cols) = syntax_error.locate_from_lexbuf lexbuf
        in
            syntax_error (message, loc, cols)

    member self.columns with get () = columns

    override self.message_header with get () = sprintf "%s column %s" base.message_header (pretty_loc columns)


type parse_error (message, token, loc, columns) =
    inherit syntax_error (message, loc, columns)

    new (message, token, lexbuf : Lexing.LexBuffer<_>) =
        let (loc, cols) = syntax_error.locate_from_lexbuf lexbuf
        in
            parse_error (message, token, loc, cols)

    member self.token with get () = token


(* Yacc parser wrapper *)

let yparse parser (tokenizer : Lexing.LexBuffer<_> -> 'tok) tokenTagToTokenId prodIdxToNonTerminal =
    let pretty_token = sprintf "<%A>"
    let pretty_token_id = tokenTagToTokenId >> sprintf "%A"
    let pretty_prod = prodIdxToNonTerminal >> sprintf "%A"
    let tokenizer lexbuf =   
        let tok =
            try tokenizer lexbuf
            with e -> raise (syntax_error (e.Message, lexbuf))
        #if DEBUG
        debug Min "%s" (pretty_token tok);
        #endif
        tok
    in
        fun (lexbuf : Lexing.LexBuffer<_>) ->
            try parser tokenizer lexbuf
            with ParseTrans.ParseErrorContext ctx as e ->
                    let ctx = ctx :?> Parsing.ParseErrorContext<'tok>
                    let tok = match ctx.CurrentToken with Some t -> pretty_token t | None -> "???"
                    let msg = ctx.Message
                    let pretty_tokens = mappen_strings pretty_token_id  " "
                    let pretty_prods = mappen_strings pretty_prod "|"
                    let shifts = pretty_tokens ctx.ShiftTokens
                    let reduces = pretty_tokens ctx.ReduceTokens
                    let states = mappen_strings (sprintf "%d") " " ctx.StateStack
                    let prods = mappen_strings pretty_prods "; " ctx.ReducibleProductions
                    let msg = sprintf "%s\n\ttoken: %s\n\tshifts: %s\n\treduces: %s\n\tstates: %s\n\treducible prods: %s" msg tok shifts reduces states prods
                    in
                        raise (parse_error (msg, tok, lexbuf))


let load_and_parse_program parse_program sourcetype filename =
    use fstr = new IO.FileStream (filename, IO.FileMode.Open)
    use rd = new IO.StreamReader (fstr)
    let lexbuf = Lexing.LexBuffer<_>.FromTextReader rd
    msg Normal "parsing %s source file '%s'..." sourcetype filename;
    let r = parse_program lexbuf
    msg Low "parsing done.";
    (r, lexbuf)


module Cobol =        

    open Absyn

    let private parse_program lexbuf =
        let prg = yparse CobolParser.program CobolLexer.datadiv_token CobolParser.tokenTagToTokenId CobolParser.prodIdxToNonTerminal lexbuf
        prg
       

    let private parse_procdiv_line =
        let next_tok = ref None
        let was_resync = ref false
        let was_eol = ref false
        in
            fun lexbuf ->
                let tokenizer lexbuf =
                    let (was_resync', was_eol', r) =                        
                        let rec f () =
                            match !next_tok with
                                Some (StatementParser.EOL as tok) -> next_tok := None; (false, true, tok)                              
                              | Some tok                          -> next_tok := None; (false, false, tok)                              
                              | None                              ->
                                    match StatementLexer.statement_token lexbuf with
                                        StatementParser.ResyncKeyword tok -> next_tok := Some tok; if !was_resync then f () else (true, false, StatementParser.RESYNC)
                                      | StatementParser.RESYNC as tok     -> if !was_resync then f () else (true, false, tok)
                                      | StatementParser.EOL as tok        -> (false, true, tok)
                                      | tok                               -> (false, false, tok)
                        in
                            f ()
                    in
                      begin
                        if (!was_resync && not was_resync') || (ParseTrans.clear_buffer_at_eol && !was_eol && not was_eol') then ParseTrans.statement_parser_token_buffer.clear;
                        was_resync := was_resync';
                        was_eol := was_eol';
                        if r <> StatementParser.RESYNC then ParseTrans.statement_parser_token_buffer.append r lexbuf;
                        r
                      end
                in
                    #if DEBUG
                    debug Min "******************";
                    #endif
                    yparse StatementParser.procdiv_line tokenizer StatementParser.tokenTagToTokenId StatementParser.prodIdxToNonTerminal lexbuf

                      
    type private statement_buffer (prg : Cobol.program) =
        let outer_sts = new Collections.Generic.List<Ideal.statement> () :> Collections.Generic.ICollection<_>
        let inner_sts = new Collections.Generic.List<Ideal.statement> () :> Collections.Generic.ICollection<_>
        let mutable curr_para = None
    with
        member private self.append_outer (st : Ideal.statement) = ignore (outer_sts.Add st)
        
        member self.append (Loc (st', (s, e)) as st : Ideal.statement) =
            ignore (inner_sts.Add st);
            prg.rows.place_range (s, e) (Cobol.row_type.from_statement' st')

        member self.regroup (start_line, end_line) lbo =
            match (inner_sts.Count, curr_para) with
                (0, None) -> ()
              | _         -> self.append_outer (Loc (Ideal.Block (curr_para, List.ofSeq inner_sts), (start_line, end_line)));
                             prg.rows.place_range (start_line, end_line) Cobol.Paragraph;
                             inner_sts.Clear ()
            curr_para <- lbo

        member self.statements
            with get () =
                #if DEBUG
                if inner_sts.Count > 0 then unexpected "parsing of statements has not finished regularly"
                #endif
                List.ofSeq outer_sts
                

    let load_and_parse_program filename =
        let (prg, lexbuf) = load_and_parse_program parse_program "COBOL" filename
        msg High "parsing procedure division...";
        let stbuf = new statement_buffer (prg)
        while not lexbuf.IsPastEndOfStream do
            lexbuf.StartPos <- lexbuf.EndPos;
            match parse_procdiv_line lexbuf with
                Cobol.ProcDiv.ActualLine (Cobol.ProcDiv.Label (Loc (lb, loc)))  -> stbuf.regroup loc (Some lb)
              | Cobol.ProcDiv.ActualLine (Cobol.ProcDiv.Statement st)           -> stbuf.append st
              | Cobol.ProcDiv.Skip                                              -> ()
              | Cobol.ProcDiv.EndOfStream                                       -> stbuf.regroup (lexbuf.StartPos.Line, lexbuf.EndPos.Line) None;
                                                                                   debug Min "%s" "end of input reached"
               
        line_feed ();
        prg.ideal_statement <- (stbuf.statements |> ParseTrans.unfold_blocks_in_statements |> ParseTrans.block_of_statements);
        prg

    let parse_pic_format = yparse CobolParser.datadiv_format CobolLexer.datadiv_format_token CobolParser.tokenTagToTokenId CobolParser.prodIdxToNonTerminal
     
        

module Ideal =
    let parse_program =
        yparse IdealParser.program IdealLexer.token IdealParser.tokenTagToTokenId IdealParser.prodIdxToNonTerminal

    let load_and_parse_program filename =
        let (prog, _) = load_and_parse_program parse_program "IL" filename
        line_feed ();
        debug Normal "parsed IL program:";
        debug Normal "%s" (Absyn.Ideal.pretty_program Absyn.Ideal.pretty_annots_annotated prog);
        line_feed ();
        prog
