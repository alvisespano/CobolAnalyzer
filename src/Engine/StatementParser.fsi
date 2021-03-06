// Signature file for parser generated by fsyacc
module CobolAnalyzer.Engine.StatementParser
type token = 
  | DOT
  | EOF
  | STOP_RUN
  | EXIT
  | MOVE
  | ADD
  | SUBTRACT
  | MULTIPLY
  | DIVIDE
  | SET
  | TO
  | FROM
  | INTO
  | BY
  | GIVING
  | GOTO
  | DEPENDING_ON
  | PERFORM
  | END_PERFORM
  | THRU
  | WITH
  | TEST
  | BEFORE
  | AFTER
  | UNTIL
  | TIMES
  | VARYING
  | THROUGH
  | COMPUTE
  | EVALUATE
  | WHEN
  | END_EVALUATE
  | IS
  | WHEN_OTHER
  | ZEROS
  | SPACES
  | INPUT
  | OUTPUT
  | NO
  | ADVANCING
  | ERROR
  | PROCEDURE
  | COPY
  | STANDARD
  | OTHER
  | ON
  | END_MOVE
  | IF
  | THEN
  | ELSE
  | END_IF
  | OF
  | TRUE
  | FALSE
  | PLUS
  | MINUS
  | MULT
  | DIV
  | MOD
  | NOT
  | AND
  | OR
  | EQ
  | LT
  | GT
  | LEQ
  | GEQ
  | NEQ
  | LESS
  | GREATER
  | THAN
  | EQUAL
  | COMMA
  | COLON
  | BRA
  | KET
  | RESYNC
  | EOL
  | END_INVARIANT
  | DOT_ACCESS of (string)
  | ResyncKeyword of (token)
  | UNKNOWN of (string)
  | INVARIANT_KEYWORD of (string)
  | KEYWORD of (string)
  | FLOAT of (float)
  | UINT64 of (uint64)
  | SINT64 of (int64)
  | ID of (Absyn.id)
  | STRING of (string)
type tokenId = 
    | TOKEN_DOT
    | TOKEN_EOF
    | TOKEN_STOP_RUN
    | TOKEN_EXIT
    | TOKEN_MOVE
    | TOKEN_ADD
    | TOKEN_SUBTRACT
    | TOKEN_MULTIPLY
    | TOKEN_DIVIDE
    | TOKEN_SET
    | TOKEN_TO
    | TOKEN_FROM
    | TOKEN_INTO
    | TOKEN_BY
    | TOKEN_GIVING
    | TOKEN_GOTO
    | TOKEN_DEPENDING_ON
    | TOKEN_PERFORM
    | TOKEN_END_PERFORM
    | TOKEN_THRU
    | TOKEN_WITH
    | TOKEN_TEST
    | TOKEN_BEFORE
    | TOKEN_AFTER
    | TOKEN_UNTIL
    | TOKEN_TIMES
    | TOKEN_VARYING
    | TOKEN_THROUGH
    | TOKEN_COMPUTE
    | TOKEN_EVALUATE
    | TOKEN_WHEN
    | TOKEN_END_EVALUATE
    | TOKEN_IS
    | TOKEN_WHEN_OTHER
    | TOKEN_ZEROS
    | TOKEN_SPACES
    | TOKEN_INPUT
    | TOKEN_OUTPUT
    | TOKEN_NO
    | TOKEN_ADVANCING
    | TOKEN_ERROR
    | TOKEN_PROCEDURE
    | TOKEN_COPY
    | TOKEN_STANDARD
    | TOKEN_OTHER
    | TOKEN_ON
    | TOKEN_END_MOVE
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_END_IF
    | TOKEN_OF
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_MULT
    | TOKEN_DIV
    | TOKEN_MOD
    | TOKEN_NOT
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_EQ
    | TOKEN_LT
    | TOKEN_GT
    | TOKEN_LEQ
    | TOKEN_GEQ
    | TOKEN_NEQ
    | TOKEN_LESS
    | TOKEN_GREATER
    | TOKEN_THAN
    | TOKEN_EQUAL
    | TOKEN_COMMA
    | TOKEN_COLON
    | TOKEN_BRA
    | TOKEN_KET
    | TOKEN_RESYNC
    | TOKEN_EOL
    | TOKEN_END_INVARIANT
    | TOKEN_DOT_ACCESS
    | TOKEN_ResyncKeyword
    | TOKEN_UNKNOWN
    | TOKEN_INVARIANT_KEYWORD
    | TOKEN_KEYWORD
    | TOKEN_FLOAT
    | TOKEN_UINT64
    | TOKEN_SINT64
    | TOKEN_ID
    | TOKEN_STRING
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startprocdiv_line
    | NONTERM_procdiv_line
    | NONTERM_newlines
    | NONTERM_sentence
    | NONTERM_statements
    | NONTERM_statement
    | NONTERM_statement'
    | NONTERM_error_statement
    | NONTERM_error_id
    | NONTERM_error_id__
    | NONTERM_anything_mode
    | NONTERM_anything
    | NONTERM_any
    | NONTERM_dotless_statements
    | NONTERM_separator
    | NONTERM_simple_statement
    | NONTERM_block_statement
    | NONTERM_invariant_stmt
    | NONTERM_invariant_kwd
    | NONTERM_anything_mode_inv_lines
    | NONTERM_anything_inv_lines
    | NONTERM_anything_inv
    | NONTERM_arith_stmt
    | NONTERM_arith_stmt_lvalue
    | NONTERM_move_stmt
    | NONTERM_perform_until_stmt
    | NONTERM_perform_call_stmt
    | NONTERM_perform_times_stmt
    | NONTERM_perform_until_varying_after_stmt
    | NONTERM_perform_varying_loop
    | NONTERM_after_varying
    | NONTERM_varying_body
    | NONTERM_perform_target
    | NONTERM_perform_target_newlines
    | NONTERM_evaluate_stmt
    | NONTERM_eval_subject
    | NONTERM_eval_body
    | NONTERM_whens
    | NONTERM_when_body
    | NONTERM_general_expr
    | NONTERM_if_stmt
    | NONTERM_set_stmt
    | NONTERM_goto_stmt
    | NONTERM_targets
    | NONTERM_target
    | NONTERM_lvalue
    | NONTERM_identifiers
    | NONTERM_compute_stmt
    | NONTERM_expr
    | NONTERM_expr__
    | NONTERM_arith_expr
    | NONTERM_string_expr
    | NONTERM_condition
    | NONTERM_condition__
    | NONTERM_cond_expr_right
    | NONTERM_cond_expr_right_item
    | NONTERM_logic_binop
    | NONTERM_arith_binop
    | NONTERM_bool_binop
    | NONTERM_bool_binop_symbol
    | NONTERM_bool_binop_keywords
    | NONTERM_num_rvalue
    | NONTERM_rvalue
    | NONTERM_structured_access
    | NONTERM_lit
    | NONTERM_num_lit
    | NONTERM_array_location
    | NONTERM_range_location
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val procdiv_line : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Absyn.Cobol.ProcDiv.line_mode) 
