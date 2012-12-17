(*
 * COBOL Type Inference
 * Absyn.fs: type representation and abstract syntax
 * (C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
#light

module CobolAnalyzer.Engine.Absyn

open System
open Prelude


(* top-level basic types *)

type id = string
type uid = string

let parse_float s = Double.Parse (s, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture)

module private NumericSize =
    let inline of_convertions (max : ^t) conv =
        let of_uint64 (u : uint64) =
            if Convert.ToUInt64 u > Convert.ToUInt64 max then unexpected "%d exceedes %d limit" u max
            else conv u

        let of_int (n : int) =
            if n < 0 then unexpected "%d is negative" n
            else n |> Convert.ToUInt64 |> of_uint64

        in
            (of_uint64, of_int)

module ArraySize =
    type t = uint32
    let parse = UInt32.Parse
    let (of_uint64, of_int) = NumericSize.of_convertions t.MaxValue Convert.ToUInt32

module FormatSize =
    type t = uint16    
    let parse = UInt16.Parse    
    let (of_uint64, of_int) = NumericSize.of_convertions t.MaxValue Convert.ToUInt16

   
(*
 * location utilities
 *)   
                     
type loc = int * int

let pretty_loc = function
    (s, e) when s = e -> sprintf "%d" s
  | (s, e)            -> sprintf "%d-%d" s e

type 'a located (x : 'a, start_line : int, end_line : int) =
    member self.start_line with get () = start_line
    member self.end_line with get () = end_line
    member self.line with get () = start_line
    member self.lines with get () = (start_line, end_line)
    member self.value with get () = x

let (|Loc|) (loc : 'a located) = (loc.value, (loc.start_line, loc.end_line))
let Loc (x : 'a, (sl, se)) = new located<'a> (x, sl, se)

let (|Unloc|) (loc : 'a located) = loc.value


type located_error (message, loc) =
    inherit System.Exception (message)

    abstract message_header : string with get
    default self.message_header with get () = sprintf "at line %s" (pretty_loc loc)

    member self.loc with get () = loc

    override self.Message with get () = sprintf "%s: %s" self.message_header message



(*
 * Idealized language AST
 *)

module Ideal =

    (* shared types & printers *)

    type lit = Int of int64
             | Float of float
             | True
             | False
             | String of string
    
    type const_value = Seq of lit list

    let rec pretty_lit = function
        Int n    -> sprintf "%d" n
      | Float x  -> sprintf "%g" x
      | True     -> "true"
      | False    -> "false"
      | String s -> sprintf "\"%s\"" s

    let pretty_const_value = function
        Seq [lit] -> pretty_lit lit
      | Seq lits  -> sprintf "[%s]" (mappen_strings pretty_lit ", " lits)


    
    (* type representation *)

    module Ty =

        type temp = TNum of bool * FormatSize.t * FormatSize.t
                  | Bool
                  (*| AlphaString of FormatSize.t
                  | AlphaNumString of FormatSize.t*)
                  | Seq of temp * ArraySize.t
                  | Type of t

        and t = Num of bool * FormatSize.t * FormatSize.t * qualifier
              | Alpha of FormatSize.t
              | AlphaNum of FormatSize.t
              | Array of t * ArraySize.t
              | Record of bindings

        and qualifier = OnePerByte | BCD | Int of uint16 | Float of uint16

        and bindings = binding list

        and binding = id * t * const_value option

        let (|AnyNumber|_|) t =
            match t with
                TNum (true, 0us, 0us) -> Some AnyNumber
              | _                     -> None

        let AnyNumber = TNum (true, 0us, 0us)

        let (|Numeric|_|) t =
            match t with
                Type (Num (sgn, n, d, _))
              | TNum (sgn, n, d)          -> Some (Numeric (sgn, n, d))
              | _                         -> None

        let (|AlphaNumeric|_|) t =
            match t with
                Type (Alpha n | AlphaNum n)
              (*| AlphaString n
              | AlphaNumString n*)          -> Some (AlphaNumeric n)
              | _                           -> None

        let pretty_num_format pretty_sign (sgn, n, d) =
            let s = pretty_sign sgn
            in
                match (n, d) with
                    _ when n > 0us && d > 0us -> sprintf "%s%d.%d" s (int n) (int d)
                  | _ when n > 0us && d = 0us -> sprintf "%s%d" s (int n)
                  | _                         -> unexpected "ill-formed num format (%b, %d, %d)" sgn (int n) (int d)

        let word_len_of_num_format (n, d) =
            let x = n + d
            if x >= 1us && x <= 4us then 16us
            elif x >= 5us && x <= 9us then 32us
            elif x >= 10us && x <= 18us then 64us
            else unexpected "%d-digits numbers are too large for binary representation" x

        let pretty_qualifier (n, d, q) =
            match q with
                OnePerByte  -> "1xb"
              | BCD         -> "bcd"
              | Int n       -> sprintf "int%d" n
              | Float n     -> sprintf "float%d" n

        let rec pretty = function
            Num (sgn, n, d, q)  -> sprintf "num.%s[%s]" (pretty_qualifier (n, d, q)) (pretty_num_format (function true -> "S" | false -> "") (sgn, n, d))
          | Alpha n             -> sprintf "alpha[%d]" (int n)
          | AlphaNum n          -> sprintf "alphanum[%d]" (int n)
          | Array (ty, n)       -> sprintf "%s array[%d]" (pretty ty) (int n)
          | Record bs           -> sprintf "{ %s }" (pretty_bindings ", " bs)
    
        and pretty_temp = function
            Bool             -> "bool"
          | AnyNumber        -> "numeric"
          | TNum (s, n, d)   -> sprintf "_num_[%s]" (pretty_num_format (function true -> "+" | false -> "-") (s, n, d))
          (*| AlphaString n    -> sprintf "alphastring[%d]" (int n)
          | AlphaNumString n -> sprintf "alphanumstring[%d]" (int n)*)
          | Seq (tty, n)     -> sprintf "%s seq[%d]" (pretty_temp tty) n
          | Type ty          -> pretty ty


        and pretty_bindings sep bs = mappen_strings pretty_binding sep bs

        and pretty_binding (x, ty, vo) = sprintf "%s : %s%s" x (pretty ty) (some_string (fun v -> sprintf " := %s" (pretty_const_value v)) vo)


    (* program representation *)

    type paragraph_id = id * id option

    type 'a annot = internal Annot of 'a * uid
    with
        static member create x uid = Annot (x, uid)

        member self.map f = match self with Annot (x, uid) -> Annot (f x, uid)

        member self.value
            with get () = match self with Annot (x, _) -> x

        member self.unique
            with get () = match self with Annot (_, uid) -> uid

    type env_bindings = Ty.bindings

    type env_binding = Ty.binding


    module Sql =

        type table_id = id
        type cursor_id = id
        type cobol_id = id
        type column_id = id

        type statement = Invariant of string * id   // siamo sicuri che servano?
                       | Unknown of string * id
                       | Include of id
                       | DeclareCursor of cursor_id * statement list
                       | DeclareTable of table_id * env_bindings
                       | Fetch of cursor_id * cobol_id list
                       | Open of cursor_id
                       | Insert of table_id * column_id list * cobol_id list
                       (*| Select of culumn_id list * table_id * column_id list*)


    type extension = Sql of Sql.statement

    type program = { procs : proc list
                     main  : body }

    and proc = { name : id
                 args : Ty.bindings
                 body : body }

    and body = statement * env_bindings
            
    and statement = statement' located

    and statement' = Assign of lvalue * expr
                   | If of expr * statement * statement option
                   | Call of id * actual list
                   | GotoAnnot of id annot
                   | Return
                   | PerformAnnot of paragraph_id annot
                   | Block of id option * statement list
                   | Unknown of string * id option * id list
                   | Invariant of string * id
                   | Nop
                   //| Ext of extension
    
    and expr = BinOp of expr * binop * expr
             | UnOp of unop * expr
             | Lit of lit
             | LValue of lvalue
    
    and lvalue = IdAnnot of id annot
               | Subscript of lvalue * expr
               | Select of lvalue * id
    
    and actual = ByRef of lvalue
               | ByVal of expr
   
    and binop = ArithBinOp of arith_binop
              | RelBinOp of rel_binop
              | LogicBinOp of logic_binop
    
    and unop = ArithUnOp of arith_unop
             | LogicUnOp of logic_unop
    
    and arith_binop = Plus | Minus | Mult | Div | Mod
   
    and arith_unop = Neg

    and rel_binop = Eq | Neq | Lt | Gt | Leq | Geq
   
    and logic_binop = And | Or 

    and logic_unop = Not
   
   

    (*
     * other pretty printers
     *)

    type pretty_annots =
      { id      : id annot -> string
        goto    : id annot -> string
        perform : paragraph_id annot -> string }

    let private tabulate tab = new String (' ', tab)

    let private retab tab = tab + 3

    let pretty_env_binding = Ty.pretty_binding

    //let pretty_annot p (Annot (x, uid)) = sprintf "%s [%s]" (p x) uid

    let pretty_env_bindings tab bs =
        let tabulation = tabulate tab
        in
            tabulation + Ty.pretty_bindings ("\n" + tabulation) bs

    let pretty_args = Ty.pretty_bindings ", "

    let pretty_paragraph_id = function
        (id, None)      -> id
      | (id1, Some id2) -> sprintf "%s %s" id1 id2
      
    let rec pretty_program pretty_annots p =
        mappen_strings (pretty_proc pretty_annots) "" p.procs + (pretty_body pretty_annots p.main)

    and pretty_body pretty_annots (st, e) = sprintf "%s\nwhere\n%s" (pretty_tabbed_statement 0 pretty_annots st) (pretty_env_bindings (retab 0) e)

    and pretty_proc pretty_annots p =
        let args = pretty_args p.args
        in
            sprintf "proc %s(%s)\n%s\n" p.name args (pretty_body pretty_annots p.body)
            
    and pretty_tabbed_statement (tab : int) pretty_annots (Loc (st, _)) = pretty_tabbed_statement' tab pretty_annots st

    and pretty_tabbed_statement' (tab : int) pretty_annots =
        let tabulation = tabulate tab
        let label = some_string (fun lb -> sprintf "%s:\n%s" lb tabulation)
        let indent_substatement (Loc (st, _)) =
            match st with
                If _ as st         -> sprintf "\n%s" (pretty_tabbed_statement' (retab tab) pretty_annots st)
                | Block (None, [st]) -> sprintf " %s" (pretty_tabbed_statement 0 pretty_annots st)
                | Block _ as st      -> sprintf "\n%s" (pretty_tabbed_statement' tab pretty_annots st)
                | st                 -> sprintf " %s" (pretty_tabbed_statement' 0 pretty_annots st)
        in
            fun st ->
                tabulation
                +
                match st with
                    Assign (lv, e)              -> sprintf "%s := %s" (pretty_lvalue pretty_annots lv) (pretty_expr pretty_annots e)
                  | If (e, tst, esto)           -> sprintf "if %s then" (pretty_expr pretty_annots e) + indent_substatement tst + some_string (fun st -> "\n" + tabulation + "else" + (indent_substatement st)) esto
                  | Call (x, acs)               -> sprintf "%s(%s)" x (mappen_strings (pretty_actual pretty_annots) ", " acs)
                  | GotoAnnot ann               -> sprintf "goto %s" (pretty_annots.goto ann)
                  | Return                      -> "return"
                  | PerformAnnot ann            -> sprintf "perform %s" (pretty_annots.perform ann)
                  | Block (lbo, [])             -> label lbo + "{}"
                  | Block (lbo, sts)            -> label lbo + sprintf "{\n%s\n%s}" (List.fold (fun s st -> s + pretty_tabbed_statement (retab tab) pretty_annots st + ";\n") "" sts) tabulation
                  | Unknown (s, kwdo, ids)      -> sprintf "? << %s >> [%s] # %s" (match kwdo with Some s -> s + " " | None -> "") (flatten_strings ", " ids) s
                  | Invariant (s, id)           -> sprintf "<< %s >> # %s" id s
                  | Nop                        -> "Nop"

    and pretty_actual pretty_annots = function
        ByVal e  -> pretty_expr pretty_annots e
      | ByRef lv -> sprintf "ref %s" (pretty_lvalue pretty_annots lv)

    and pretty_expr pretty_annots = function
        BinOp (e1, op, e2) -> sprintf "%s %s %s" (pretty_expr pretty_annots e1) (pretty_binop op) (pretty_expr pretty_annots e2)
      | UnOp (op, e)       -> sprintf "%s %s" (pretty_unop op) (pretty_expr pretty_annots e)
      | Lit lit            -> pretty_lit lit
      | LValue lv          -> pretty_lvalue pretty_annots lv

    and pretty_lvalue pretty_annots = function
        IdAnnot ann         -> pretty_annots.id ann
      | Subscript (lv, e)   -> sprintf "%s[%s]" (pretty_lvalue pretty_annots lv) (pretty_expr pretty_annots e)
      | Select (lv, x)      -> sprintf "%s.%s" (pretty_lvalue pretty_annots lv) x
   
    and pretty_binop = function
        ArithBinOp op -> pretty_arith_binop op
      | RelBinOp op   -> pretty_rel_binop op
      | LogicBinOp op -> pretty_logic_binop op

    and pretty_unop = function
        ArithUnOp op -> pretty_arith_unop op
      | LogicUnOp op -> pretty_logic_unop op

    and pretty_arith_binop = function
        Plus    -> "+"
      | Minus   -> "-"
      | Mult    -> "*"
      | Div     -> "/"
      | Mod     -> "%"

    and pretty_arith_unop = function
        Neg -> "-"

    and pretty_rel_binop = function
        Eq    -> "="
      | Neq   -> "<>"
      | Lt    -> "<"
      | Gt    -> ">"
      | Leq   -> "<="
      | Geq   -> ">="

    and pretty_logic_binop = function
        And  -> "and"
      | Or   -> "or"
    
    and pretty_logic_unop = function
        Not -> "not"

    (* shortcuts *)

    let pretty_statement st = pretty_tabbed_statement 0 st
    let pretty_statement' st = pretty_tabbed_statement' 0 st

    let pretty_annots_annotated =
        let pretty_annot p (Annot (x, uid)) = sprintf "%s [%s]" (p x) uid
        in
            { id = pretty_annot identity;
              goto = pretty_annot identity;
              perform = pretty_annot pretty_paragraph_id }

    let pretty_annots_invisible =
        let pretty_annot p (Annot (x, _)) = p x
        in
            { id = pretty_annot identity;
              goto = pretty_annot identity;
              perform = pretty_annot pretty_paragraph_id }

    //let pretty_program_untyped = pretty_program { id = pretty_annot identity; goto = pretty_annot identity; perform = pretty_annot pretty_paragraph_id }

    (*
     * abstract domains
     *)

    module Dom =
         
        module Numeric =                   
            
            type norm () = class end
        
            type U () = inherit norm ()
            type N () = inherit norm ()

            type monome<'x when 'x :> norm> = private Monome of float * (id * int) list
            with
                static member make (k, vs) : U monome = Monome (k, vs)
                static member make k = monome<_>.make (k, [])
                (* basic *)         
                static member get_Zero () : 'n monome = Monome (0.0, []) 
                static member (+) (m1 : monome<_>, m2 : monome<_>) : U polynome = Polynome [m1.normalize; m2.normalize] 
                static member (~-) (Monome (k, vs) : 'n monome) : 'n monome = Monome (-k, vs)
                static member (*) (Monome (k1, vs1) : monome<_>, Monome (k2, vs2) : monome<_>) : U monome = Monome (k1 * k2, vs1 @ vs2)
                static member Pow (Monome (k, vs) : 'n monome, e : int) : 'n monome = Monome (k ** (float e), List.map (fun (x, n) -> (x, n * e)) vs)
                (* derived *)
                static member (-) (m1 : monome<_>, m2 : monome<_>) = m1 + -m2
                static member (/) (m1 : monome<_>, m2 : monome<_>) = m1 * (m2 ** -1)

                member self.normalize : N monome =
                    match self with
                      Monome (k, _) when equal_float k 0.0 -> monome<_>.get_Zero ()
                    | Monome (k, [])                       -> Monome (k, [])
                    | Monome (k, vs) ->                  
                        let vs' =
                            vs |> List.fold (fun map (x, n) -> Map.add x (match Map.tryFind x map with Some n' -> n + n' | None -> n) map) Map.empty
                               |> Map.toList |> List.filter (fun (x, n) -> n <> 0)
                        in
                            Monome (k, vs') 

            and polynome<'x when 'x :> norm> = private Polynome of N monome list
            with
                static member make kvs : U polynome = Polynome (List.map (fun (k, vs) -> (monome<_>.make (k, vs)).normalize) kvs)
                static member make k = polynome<_>.make [k, []]
                (* basic *)                 
                static member get_Zero () : 'n polynome = Polynome [monome<_>.get_Zero ()]
                static member (+) (Polynome ms1, Polynome ms2) : U polynome  = Polynome (ms1 @ ms2)
                static member (~-) (Polynome ms : 'n polynome) : 'n polynome = Polynome (List.map (fun m -> -m) ms)
                static member (*) (Polynome ms1, Polynome ms2) : U polynome = List.sumBy (fun (m1 : N monome) -> List.sumBy (fun (m2 : N monome) -> Polynome [(m1 * m2).normalize]) ms2) ms1

                static member Pow (p : 'n polynome, e : int) : U polynome =
                    let rec f n = if n = 0 then polynome<_>.make 1.0 else p * (f (n - 1))
                    in
                        if e >= 0 then f e
                        else
                            match f -e with
                                Polynome ms -> Polynome (List.map (fun m -> m ** -1) ms)

                (* derived *)
                static member (-) (p1 : polynome<_>, p2 : polynome<_>) = p1 + -p2
                static member (/) (p1 : polynome<_>, p2 : polynome<_>) = p1 * (p2 ** -1)
                
                member self.normalize : N polynome =
                    match self with
                        Polynome mons ->
                            let mons' =
                               mons |> List.map (fun m -> m.normalize)
                                    |> List.fold (fun map (Monome (k, vars)) -> Map.add vars (either 0.0 (map.TryFind vars) + k) map) Map.empty
                                    |> Map.toList |> List.map (fun (vars, k) -> Monome (k, vars))
                                    |> List.filter (fun (Monome (k, vars)) -> not (equal_float k 0.0))
                            in
                                Polynome mons'
                          
                member self.pretty =
                    match self with
                      Polynome [] -> "0"
                    | Polynome mons->
                        let p (Monome (k, vars)) =
                            let sign = if k < 0.0 then "-" else "+"
                            let coeff_part = let k = Math.Abs k in if equal_float k 1.0 then "" else sprintf "%g" k
                            let (posexps, negexps) = separate2 (fun (x, n) -> if n >= 0 then Choice1Of2 (x, uint32 n) else Choice2Of2 (x, uint32 -n)) vars
                            let p_vars = function
                                [] -> ""
                                | vs ->
                                    let p = function
                                        (_, 0u) -> ""
                                        | (x, 1u) -> x
                                        | (x, 2u) -> sprintf "%s²" x
                                        | (x, 3u) -> sprintf "%s³" x
                                        | (x, n)  -> sprintf "%s^%d" x n
                                    in
                                        mappen_strings p "·" vs
                            let numer_part = if posexps.Length > 0 then "·" + p_vars posexps else ""
                            let denom_part = if negexps.Length > 0 then " / " + p_vars negexps else ""
                            in
                                sprintf "%s %s%s%s" sign coeff_part numer_part denom_part
                        in
                            mappen_strings p " " mons
                                   
            
            module private Test =

                let test () =
                    let p0 = polynome<_>.make [2.0, [("a", 1); ("b", 2)]]
                    let p1 = p0 + p0 * p0
                    let p2 = p1 / (p1 + p0)
                    let p3 = p2 - p1 * p2
                    p3
                
                   
        module Boolean =
            open Numeric
                
            type system =
                  { formula : polynome<N>
                    context : Map<id, Numeric.polynome<N> * rel_binop> }
                    
        type 'a tree_node =
          { condition   : Boolean.system
            then_branch : 'a tree
            else_branch : 'a tree }

        and 'a tree = Node of 'a tree_node
                    | Leaf of 'a

        type t = Num of Numeric.polynome<N> tree
               | Alpha of string tree
               | Bool of Boolean.polynome<N> tree
     

        let pretty = function
            Num n   -> Numeric.pretty n
          | Alpha a -> Alphabetic.pretty a
          | Bool b  -> Boolean.pretty b



(*
 * COBOL division ASTs
 *)

module Cobol =

    type lit = Int of int64
             | Float of float
             | String of string


    module IdDiv =
        type t = id located


    module EnvDiv =
        type t = unit located


    module DataDiv =
        type tab_type = uint16
        type occurs_type = ArraySize.t

        type alpha_format_char = A | X
        type num_format_char = Nine | Zero | Z
        
        type format = AlphaFormat of alpha_format_char list
                    | NumFormat of bool * num_format_char list * num_format_char list

        type qualifier = Comp | Comp1 | Comp2 | Comp3 | Binary | PackedDecimal

        type value = Seq of lit list
                   | Thru of (lit * lit) list

        type line_kind = Record
                       | Picture of string * qualifier option * value option
        
        type line = Line of tab_type * id * line_kind * occurs_type
        
        type t = line located list located


    module ProcDiv =
        type line = Label of string located
                  | Statement of Ideal.statement

        type line_mode = ActualLine of line | Skip | EndOfStream


    type row_type = UnknownStatement
                  | KnownStatement
                  | Division
                  | Picture
                  | PictureArray
                  | Record
                  | RecordArray
                  | Paragraph
                  | Default
    with
        static member internal from_statement' = function
            Ideal.Unknown _ -> UnknownStatement
          | _               -> KnownStatement

        static member internal from_datadiv_line (DataDiv.Line (_, _, k, occurs)) =
            match k, occurs with
                DataDiv.Record, 1u      -> Record
              | DataDiv.Record, _       -> RecordArray
              | DataDiv.Picture _, 1u   -> Picture
              | DataDiv.Picture _, _    -> PictureArray


    type row_list () =
        inherit Collections.Generic.List<row_type> ()
    with
        member self.place row rt =
            let len = self.Count
            in                
                if row >= len then
                    (for i = len to row - 1 do self.Add Default);
                    self.Add rt
                else
                    self.[row] <- rt

        member self.place_range (s, e) rt =
            for row = s to e do self.place row rt


    type program (id : IdDiv.t, env : EnvDiv.t, data : DataDiv.t, proc : unit located) =
        let mutable st : Ideal.statement option = None
        let _rows = new row_list ()

        do
            let place_as_div (Loc (_, loc)) = _rows.place_range loc Division
            place_as_div id;
            place_as_div env;
            place_as_div data;
            place_as_div proc;
            List.iter (function Loc (_, (s, e)) -> _rows.place_range (s, e) Picture) data.value
    with
        member self.identification
            with get () = id

        member self.environment
            with get () = env

        member self.data
            with get () = data

        member self.ideal_statement
            with get () = Option.get st
            and internal set st' = st <- Some st'

        member self.rows
            with get () = _rows
        