(*
 * COBOL Type Inference
 * TypeChecker.fs: type inference & checker
 * (C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
#light

module CobolAnalyzer.Engine.Typing

open System
open System.Text.RegularExpressions
open Absyn
open Absyn.Ideal
open Prelude
open Log
open StateMonad

(* TODO

    - verificare numeri di riga nell'output
    - verificare il pretty printing degli unknown statement
    - cambiare l'ordine giusto del logging in modo che le ricorsioni non lo incasinino
    - prettyprintare meglio il topological env con i current e non solo gli original
*)



(* type error reporting *)

exception TypeError of string
exception UnboundSymbolError of string

type type_error (message, loc) =
    inherit located_error (message, loc)


module Report =

    let throw_type_error fmt = throw_formatted TypeError fmt

    let unbound_symbol id = raise (UnboundSymbolError id)

    let mismatch et t where = throw_type_error "expected type '%s' in %s, but got '%s'" et where t

    let temporary_mismatch et t where = throw_type_error "expected type '%s' in %s, but got '%s'" (Ty.pretty_temp et) where (Ty.pretty_temp t)

    let mismatch_subtyped et t where =
        let et = Ty.pretty et
        let t = Ty.pretty t
        in
            throw_type_error "expected a subtype of '%s' as %s, but got '%s'" et where t

    let bin_mismatch et1 et2 t1 t2 where =
        let et1 = Ty.pretty_temp et1
        let et2 = Ty.pretty_temp et2
        let t1 = Ty.pretty_temp t1
        let t2 = Ty.pretty_temp t2
        in
            throw_type_error "expected types '%s' and '%s' in %s but got '%s' and '%s'" et1 et2 where t1 t2

    let un_mismatch et t where =
        let et = Ty.pretty_temp et
        let t = Ty.pretty_temp t
        in
            throw_type_error "expected type '%s' in %s but got '%s'" et where t

    let binop_mismatch et1 et2 t1 t2 op =
        let where = sprintf "binary operator '%s'" (pretty_binop op)
        in
            bin_mismatch et1 et2 t1 t2 where

    let unop_mismatch et t op =
        let where = sprintf "unary operator '%s'" (pretty_unop op)
        in
            un_mismatch et t where

    let out_of_bounds tya i =
        let tya = Ty.pretty tya
        in
            throw_type_error "subscript expression evaluates to constant '%d' which is out of '%s' bounds" i tya

    let array_mismatch t =
        let t = Ty.pretty t
        in
            throw_type_error "expected an array type at left side of subscript but got '%s'" t
   
    let undefined_record_label lb r =
        let r = Ty.pretty r
        in
            throw_type_error "undefined label '%s' in record '%s'" lb r

    let filler_in_select lv =
        throw_type_error "l-value '%s' attempts to access record field labeled as filler" (pretty_lvalue pretty_annots_invisible lv)

    let record_expected t lb =
        let tlv = Ty.pretty t
        in
            throw_type_error "type '%s' is not a record type with label '%s'" tlv lb

    let etherogeneous_seq ty ty1 lit =
        let ty = Ty.pretty_temp ty
        let ty1 = Ty.pretty_temp ty1
        let lit = pretty_lit lit
        in
            throw_type_error "etherogeneous literal sequence: expected literals of type '%s' but '%s' has type '%s'" ty lit ty1

    let ambiguous_choice x fi =
        throw_type_error "ambiguous choice detected: %s : %s" x fi


    module Warning =

        let binop_unsafe_operation t1 t2 op =
            let where = sprintf "binary operator '%s'" (pretty_binop op)
            let t1 = Ty.pretty_temp t1
            let t2 = Ty.pretty_temp t2
            in
                warn Normal "possible unsafe operation between types '%s' and '%s' in %s" t1 t2 where

        let incompatible_argument p arg targ tactual comparison result =
            let targ = Ty.pretty targ
            let tactual = Ty.pretty tactual
            in
                warn Normal "actual parameter of type '%s' %s type '%s' as argument '%s' of procedure '%s': %s" tactual comparison targ arg p result

        let truncated_argument_byval p arg targ tactual =
            incompatible_argument p arg targ tactual "is a supertype of (i.e. bigger than)" "callee value will be truncated at runtime"

        let subtyped_argument p arg targ tactual =
            incompatible_argument p arg targ tactual "is a subtype of (i.e. smaller than)" "call behaviour is unpredictable"

        let supertyped_argument_byref p arg targ tactual =
            incompatible_argument p arg targ tactual "is a supertype of (i.e. bigger than)" "references are not truncatedbut call behaviour is unpredictable"

        let truncated_subsumption t fi =
            warn Normal "truncated subsumption detected: one or more choices of choice type '%s' do not fit into original type '%s'" fi (Ty.pretty t)

        let truncated_fit t1 t2 =
            warn Normal "truncation detected in fitting type '%s' into type '%s'" (Ty.pretty t1) (Ty.pretty t2)

        let corrupted_fit t1 t2 =
            warn Normal "corrupted data detected in fitting type '%s' into type '%s'" (Ty.pretty t1) (Ty.pretty t2)


(* type comparison functions *)

module TyCmp =
    open Ty

    let rec private rep = function
        Num (_, n, d, OnePerByte)              -> uint64 (n + d)
      | Num (_, n, d, BCD)                     -> uint64 (let l = n + d + 1us in (l + l % 2us) / 2us)
      | Num (_, _, _, (Int bits | Float bits)) -> uint64 (bits / 8us)
      | Alpha n | AlphaNum n                   -> uint64 n
      | Array (ty, n)                          -> (uint64 n) * (rep ty)
      | Record bs                              -> List.map (fun (_, ty, _) -> rep ty) bs |> List.sum
                       
    let compare t1 t2 = compare (rep t1) (rep t2)

    let is_subtype t1 t2 = compare t1 t2 <= 0

    let rec is_bindable t temp =
        let (==>) a b = a || not b
        match (t, temp) with
            (Num (sgn1, n1, d1, _), TNum (sgn2, n2, d2)) -> sgn1 ==> sgn2 && n2 <= n1 && d2 <= d1

          //| ((Alpha n1 | AlphaNum n1), (AlphaString n2 | AlphaNumString n2)) -> n2 <= n1
          | ((Alpha n1 | AlphaNum n1), (Type (Alpha n2) | Type (AlphaNum n2))) -> n2 <= n1

          | (AlphaNum n1, TNum (false, n2, d2)) -> n1 <= n2 + d2
          | (AlphaNum n1, TNum (true, n2, d2))  -> n1 <= 1us + n2 + d2

          //| (Array (t1, n1), Seq (t2, n2)) -> is_bindable t1 t2 && n1 = n2
          | (Array (t1, n1), Type (Array (t2, n2))) -> is_bindable t1 (Type t2) && n1 = n2

          //| (Record bs1, Record bs2) -> List.forall (fun (x1, t1, _) -> List.exists (fun (x2, t2, _) -> x1 = x2 && is_bindable t1 t2) bs2) bs1

          | _ -> false

    let rec promote_temporary t temp =
        match (t, temp) with
            (Num (_, _, _, q), TNum (sgn, n, d))    -> Num (sgn, n, d, q)
          | (_, TNum (sgn, n, d))                   -> Num (sgn, n, d, Ty.OnePerByte)
          (*| (_, AlphaString n)                      -> Alpha n
          | (_, AlphaNumString n)                   -> AlphaNum n*)
          | (_, Bool)                               -> promote_temporary t (TNum (false, 1us, 0us))
          | (_, Type t2)                            -> t2
          | (_, Seq _)                              -> unexpected "type '%s' cannot appear as r-value in assignment" (pretty_temp temp)

    let fit t t1 =
        let warn_trunc_if b r = (if b then Report.Warning.truncated_fit t t1); r
        let warn_corr_if b r = (if b then Report.Warning.corrupted_fit t t1); r
        match (t, t1) with
          | Num (_, n, d, (OnePerByte | BCD)), Num (_, n1, d1, (OnePerByte | BCD)) -> warn_trunc_if (n1 < n || d1 < d) t1
          | Num (_, n, d, (OnePerByte | BCD)), Num (_, n1, d1, Float 32us)         -> warn_corr_if (n > Config.Typing.Layout.float32_max_integral_digits || d > Config.Typing.Layout.float32_max_fractional_digits) t1
          | Num (_, n, d, (OnePerByte | BCD)), Num (_, n1, d1, Float 64us)         -> warn_corr_if (n > Config.Typing.Layout.float64_max_integral_digits || d > Config.Typing.Layout.float64_max_fractional_digits) t1
          | Num (_, n, d, (OnePerByte | BCD)), Num (_, _, _, Float _)              -> unexpected "fit: floating-point numeric type '%s' is not supported" (Ty.pretty t1)

          | Num (_, n, d, OnePerByte), Num (_, _, _, Int _) when d > 0us       -> warn_corr_if true t1
          | Num (_, n, d, OnePerByte), Num (_, n1, _, Int _)                   -> warn_trunc_if (n1 < n) t1
          | Num (_, n, d, OnePerByte), (Alpha n1 | AlphaNum n1) when d > 0us   -> warn_corr_if true t1
          | Num (_, n, d, OnePerByte), (Alpha n1 | AlphaNum n1)                -> warn_trunc_if (n + d > n1) t1

          | Num (_, n, d, BCD), Num (_, n1, _, Int _)    -> warn_trunc_if (d > 0us || n1 < n) t1
          | Num (_, n, d, BCD), (Alpha n1 | AlphaNum n1) -> warn_corr_if true t1
          
          | Num (_, _, _, Int 64us), Num (_, _, _, Float 32us) -> warn_corr_if true t1

          | Num (_, n, d, Int _), Num (_, _, _, Int _)               -> warn_trunc_if  (rep t > rep t1) t1
          | Num (_, n, d, Int _), Num (_, n1, _, (OnePerByte | BCD)) -> warn_trunc_if (n1 < n) t1
          | Num (_, n, d, Int _), Num (_, _, _, Float _)             -> t1
          | Num (_, n, d, Int _), (Alpha n1 | AlphaNum n1)           -> warn_corr_if true t1
          
          | Num (_, _, _, Float 64us), Num (_, _, _, Float 32us) -> warn_trunc_if true t1

          | Num (_, n, d, Float _), Num (_, n1, d1, OnePerByte) -> warn_trunc_if (n1 < n || d1 < d) t1
          | Num (_, n, d, Float _), Num (_, n1, d1, BCD)        -> warn_corr_if true t1
          | Num (_, n, d, Float _), Num (_, _, _, Int _)        -> warn_trunc_if true t1
          | Num (_, n, d, Float _), Num (_, _, _, Float _)      -> t1
          | Num (_, n, d, Float _), (Alpha n1 | AlphaNum n1)    -> warn_corr_if true t1
          
          | Alpha n, Num (_, _, _, _)                           -> warn_corr_if true (Alpha (FormatSize.of_uint64 (rep t1)))
          | AlphaNum n, Num (_, _, _, _)                        -> warn_corr_if true (AlphaNum (FormatSize.of_uint64 (rep t1)))
          | (Alpha n | AlphaNum n), (Alpha n1 | AlphaNum n1)    -> warn_trunc_if (n1 < n) t1

          | _, (Array _ | Record _)
          | (Array _ | Record _), _ -> not_implemented "fit: type '%s' into type '%s'" (Ty.pretty t) (Ty.pretty t1)




(* partial ordering *)

type IPartiallyComparable<'a> =
    abstract PartialCompareTo : 'a -> int option


(* choice type definition *)

type choice_type (set : Ty.t Set) =
    let _set = set
    
    interface IComparable with
        member self.CompareTo o = compare _set (o :?> choice_type).set
    
    interface IPartiallyComparable<choice_type> with
        member self.PartialCompareTo fi =
            if set = fi.set then Some 0
            elif set.IsProperSupersetOf fi.set then Some -1
            elif set.IsProperSubsetOf fi.set then Some 1
            else None

    override self.Equals fi = (self :> IComparable).CompareTo fi = 0

    override self.GetHashCode () = _set.GetHashCode ()

    new t = choice_type (Set.singleton t)

    member internal self.set = _set

    member self.map f = choice_type (Set.map f _set)

    member self.merge (fi : choice_type) = choice_type (Set.union _set fi.set)

    member self.pretty = mappen_strings Ty.pretty "|" _set

    member self.is_subtype_of t2 = Set.forall (fun t1 -> TyCmp.is_subtype t1 t2) _set

    

let (|Single|Choice|) (fi : choice_type) =
    let sz = Set.count fi.set
    in
        if sz > 1 then Choice fi.set
        elif sz = 1 then Single (Seq.head (Set.toSeq fi.set))
        else raise (Unexpected "empty set in choice type")




(* flow type definitions *)

type flow_type =
    { current  : choice_type
      original : Ty.t
      domain   : Dom.t
    }
with          
    static member init t dom =
        { original = t;
          current  = new choice_type (t);
          domain   = dom;
        }
                      
    member self.coerce (fi : choice_type) =
        if not (fi.is_subtype_of self.original) then Report.Warning.truncated_subsumption self.original fi.pretty;
        { self with current = fi.map (fun t -> TyCmp.fit t self.original) }
      
    member self.merge ft =
        if self.original <> ft.original then unexpected "'%s' and '%s' cannot be merged" (Ty.pretty self.original) (Ty.pretty ft.original);
        { self with current = self.current.merge ft.current }

    member self.pretty =
        match self.current with
            Single t when t = self.original -> self.current.pretty
          | _                               -> let dir = if self.current.is_subtype_of self.original then "<:" else ":>"
                                               sprintf "%s %s %s @ %s" self.current.pretty dir (Ty.pretty self.original) (Dom.pretty self.domain)
   
        

(* constant expression evaluator *)

let rec eval_const_expr = function
    BinOp (e1, ArithBinOp op, e2) ->
        let v1 = eval_const_expr e1
        let v2 = eval_const_expr e2
        in
            match (v1, v2) with
                (Some n1, Some n2) ->
                    let f =
                        match op with
                            Plus  -> (+)
                          | Minus -> (-)
                          | Mult  -> (*)
                          | Div   -> (/)
                          | Mod   -> (%)
                    in
                        Some (f n1 n2)

              | _ -> None

  | UnOp (ArithUnOp Neg, e) ->
        match eval_const_expr e with
            Some n -> Some (-n)
          | None   -> None

  | Lit (Int n) -> Some n
        
  | _ -> None



(* type environment *)

module Env =
    type 'a combine_mode = Existant of id * 'a * 'a
                         | New of id * 'a

    type 'a t = Env of Map<id, 'a>
    
    let search x (Env m) = Map.tryFind x m

    let lookup x m =
        match search x m with
            Some x -> x
          | None   -> Report.unbound_symbol x

    let bind x t (Env m) = Env (Map.add x t m)
    
    let append (Env m2) (Env m1) = Env (Map.fold (fun m x t -> Map.add x t m) m1 m2)

    let diff f z (Env m2) (Env m1) =
        let f z x v2 =
            let esit = 
                match Map.tryFind x m1 with
                    Some v1 -> Existant (x, v1, v2)
                  | None    -> New (x, v2)
            in
                f z esit
        in
            Map.fold f z m2

    let combine f env2 env1 =
        let g env' = function
            (Existant (x, _, _) | New (x, _)) as esit ->
                match f esit with
                    Some v -> bind x v env'
                  | None   -> env'
        in
            diff g env1 env2 env1

    let filter f (Env m) = Env (Map.filter f m)

    let pretty p (Env m) =
        if m.IsEmpty then "<empty>"
        else
            let ss = Map.fold (fun ss x v -> p x v :: ss) [] m
            in
                flatten_strings "\n" ss

    let pretty_diffs pretty_existant pretty_new env2 env1 =
        let f ss = function
            Existant (x, v1, v2) -> if v1 <> v2 then pretty_existant x v1 v2 :: ss else ss
          | New (x, v)           -> pretty_new x v :: ss
        in
            diff f [] env2 env1

    let empty = Env Map.empty

    let partial_compare (Env m1) (Env m2) =
        let cmp op m1 m2 = Map.forall (fun x1 v1 -> match Map.tryFind x1 m2 with Some v2 -> op v1 v2 | None -> false) m1
        in
            if m1.Count = m2.Count && cmp (=) m1 m2 then Some 0
            elif cmp (>) m2 m1 then Some -1
            elif cmp (>) m1 m2 then Some 1
            else None



module VarEnv =    
    type t = flow_type Env.t

    let bind_single x (ty, dom) env = Env.bind x (flow_type.init ty dom) env
    let bind_singles bs env = List.fold (fun env (x, t, _) -> bind_single x t env) env bs   
    let bind_args = bind_singles

    let merge env1 env2 =
        let f = function
            Env.Existant (x, ft1 : flow_type, ft2) -> Some (ft1.merge ft2)
          | Env.New (_, ft)                        -> Some ft
        in
            Env.combine f env2 env1
    
    let pretty venv = Env.pretty (fun x (ft : flow_type) -> sprintf "%s : %s" x ft.pretty) venv

    let pretty_diffs venv' venv =
        let pretty_existant x (ft1 : flow_type) (ft2 : flow_type) = sprintf "%s : %s ==> %s" x ft1.pretty ft2.pretty
        let pretty_new x (ft : flow_type) = sprintf "%s : %s" x ft.pretty
        in
            Env.pretty_diffs pretty_existant pretty_new venv' venv


module TopoEnv =
    type t = flow_type Env.t

    let pretty env = Env.pretty (fun x (ft : flow_type) -> sprintf "%s : %s" x ft.pretty) env


module BranchEnv =
    type t = TopoEnv.t Env.t


module LabelEnv =
    type t = statement list Env.t

    let rec bind_statements sts (lenv : t) =
        match sts with
            [] -> lenv

          | (st :: sts) as allsts ->
                let lenv' =
                    match st with
                        Loc (Block (Some lb, _), _) -> Env.bind lb allsts lenv
                      | _                           -> lenv
                in
                    bind_statements sts lenv'

module ProcEnv =
    type signature =
      { args   : Ty.bindings;
        argenv : VarEnv.t;
      }

    type t = signature Env.t


(* state monad access *)

type environments =
  { var     : VarEnv.t;
    topo    : TopoEnv.t;
    branch  : BranchEnv.t;
    label   : LabelEnv.t;
    proc    : ProcEnv.t }

let get_var_env envs = (envs.var, envs)
let get_topo_env envs = (envs.topo, envs)
let get_label_env envs = (envs.label, envs)
let get_branch_env envs = (envs.branch, envs)

let set_var_env venv envs = (), { envs with var = venv }
let set_topo_env tenv envs = (), { envs with topo = tenv }

let lift_var_env f envs = (), { envs with var = f envs.var }
let lift_topo_env f envs = (), { envs with topo = f envs.topo }
let lift_label_env f envs = (), { envs with label = f envs.label }
let lift_branch_env f envs = (), { envs with branch = f envs.branch }



(* type checker *)

let rec typecheck_program prg =
    let penv = List.fold (fun penv p -> Env.bind p.name (typecheck_proc penv p) penv) Env.empty prg.procs
    typecheck_body Env.empty penv prg.main


and typecheck_proc (penv : ProcEnv.t) p =
    msg Normal "entering procedure %s(%s)..." p.name (pretty_args p.args);
    { ProcEnv.args = p.args;

      ProcEnv.argenv =
        let envs = typecheck_body (VarEnv.bind_args p.args Env.empty) penv p.body
        msg Low "typing of procedure %s(%s) done." p.name (pretty_args p.args);
        Env.filter (fun x _ -> List.exists (fun (x', _, _) -> x = x') p.args) envs.var;
    }
  
and typecheck_body venv penv (st, env) =
    let venv0 = VarEnv.bind_singles (typecheck_env_bindings env) venv
    let lenv0 = LabelEnv.bind_statements [st] Env.empty
    let ((), envs) = typecheck_statement None st { topo = Env.empty; var = venv0; proc = penv; branch = Env.empty; label = lenv0 }
    msg High "final topological env:\n%s" (TopoEnv.pretty envs.topo);
    envs

and typecheck_env_bindings bs = List.map typecheck_env_binding bs

and typecheck_env_binding ((x, ty, vo) as b) =
    match vo with
        None -> ()
      | Some v ->
            let (tv, dom) = typecheck_const_value v
            in
                if not (TyCmp.is_bindable ty tv) then Report.mismatch (Ty.pretty ty) (Ty.pretty_temp tv) (sprintf "binding of symbol '%s'" x);
    b


and typecheck_const_value = function
    Seq []    -> unexpected "empty seq literal"
  | Seq [lit] -> typecheck_lit lit

  | Seq lits ->
        let tys = List.map (fun lit -> typecheck_lit lit |> fst) lits
        let ty1 = List.head tys
        in
            List.iteri (fun i ty -> if ty <> ty1 then Report.etherogeneous_seq ty ty1 (List.nth lits i)) tys;
            (Ty.Seq (ty1, ArraySize.of_int lits.Length), not_implemented "typecheck_const_value seq")

     

and typecheck_statement stoplb (Loc (stmt', loc)) =
  st {
    line_feed ();
    msg High "line %s:" (pretty_loc loc);
    try do! typecheck_statement' stoplb stmt'
    with TypeError s           -> raise (type_error (s, loc))
       | UnboundSymbolError id -> raise (type_error (sprintf "unbound symbol '%s'" id, loc))
  }
   

and typecheck_branch (startlb, stoplb) uid =
  st {
    let! benv = get_branch_env
    let! tenv = get_topo_env
    match Env.search uid benv with
          Some tenv' when
                (debug Normal "current topological env:\n%s\nstored topological env:\n%s" (TopoEnv.pretty tenv) (TopoEnv.pretty tenv');
                    match Env.partial_compare tenv' tenv with
                      Some 0 -> hint High "topological environment has fully converged"; true
                    | Some n -> if n < 0 then hint High "topological environment seems converging"; true
                                else warn High "topological environment seems diverging"; false
                    | None   -> false) ->
            ()

        | _ ->
            debug Normal "storing topological env:\n%s" (TopoEnv.pretty tenv);
            do! lift_branch_env (Env.bind uid tenv)
            msg High "branching to block '%s'" startlb;
            let! lenv = get_label_env
            do! typecheck_statements stoplb (Env.lookup startlb lenv)
    }


and typecheck_statements stoplb =
    let rec loop sts =
      st {
        //let! tenv = get_topo_env in debug Unmaskerable "topological env:\n%s" (TopoEnv.pretty tenv)
        match sts with
            [] -> ()

          | [Unloc Return] ->
                debug Min "return statement at end of block"

          | Unloc Return :: _ ->
                warn Unmaskerable "return statement followed by something in block"

          | st0 :: sts ->
                msg Low "typecheking statement:\n%s" (Ideal.pretty_tabbed_statement 4 Ideal.pretty_annots_annotated st0);
                let! venv = get_var_env
                debug Low    "[before] %s" (VarEnv.pretty venv);
                do! try typecheck_statement None st0
                    with :? type_error as e when Config.Typing.tolerant -> recoverable_error "%s" e.Message; set_var_env venv
                let! venv = get_var_env
                debug Normal "[here]   %s" (VarEnv.pretty venv);
                let! venv' = get_var_env
                debug Low    "[after]  %s" (VarEnv.pretty venv');
                debug Normal "[diff]   %s" (VarEnv.pretty_diffs venv' venv |> flatten_strings "; ");
                
                // if current statement is a paragraph stop-point then return control to caller, else loop on
                match (st0, stoplb) with
                    (Unloc (Block (Some lb, _)), Some stoplb) when lb = stoplb -> debug High "ending block '%s' reached" lb
                  | _                                                          -> do! loop sts
      }
    in
        fun sts ->
          st {
            do! lift_label_env (LabelEnv.bind_statements sts)
            do! loop sts
          }


and typecheck_statement' stoplb stmt =
  st {
   match stmt with
    Unknown (_, kwdo, ids) ->
        let! envs = get_state
        let ids = List.filter (fun id -> Option.isSome (Env.search id envs.var)) ids
        let kwd = either "?" kwdo
        warn High "unknown statement '%s' might alter flow types of the following variables: %s" kwd (flatten_strings ", " ids)


  | Invariant (_, id) ->
        msg Low "invariant command '%s' does not alter flow types" id


  | Nop -> ()


  | Assign (lv, e) -> 
        let! (te, dome) = typecheck_expr e
        let! (tlv, domlv, coerce) = typecheck_lvalue lv
        do! coerce (new choice_type (TyCmp.promote_temporary tlv te))
                  
                      
  | GotoAnnot ann ->
        Option.iter (fun stoplb -> warn High "goto statement encountered while executing a perform thru label '%s': control seems to jump away" stoplb) stoplb;
        do! typecheck_branch (ann.value, stoplb) ann.unique;
        debug Low "control returned after goto: '%s'" (pretty_statement' pretty_annots_annotated stmt)
                

  | Return -> unexpected "return"


  | PerformAnnot ann ->
        do! typecheck_branch ann.value ann.unique
        debug Low "control returned after perform: '%s'" (pretty_statement' pretty_annots_annotated stmt)
        

  | Block (lbo, sts) ->
        do! typecheck_statements stoplb sts
        
        
  | If (e, st1, st2o) -> 
        let! (te, _) = typecheck_expr e
        in
            match te with
                Ty.Bool -> 
                    let! venv0 = get_var_env
                    do! typecheck_statement stoplb st1
                    let! venv1 = get_var_env
                    do! set_var_env venv0
                    do! some_state (typecheck_statement stoplb) st2o
                    let! venv2 = get_var_env
                    do! set_var_env (VarEnv.merge venv1 venv2)

              | _ -> Report.temporary_mismatch Ty.Bool te "if statement condition"


  | Call (p, actuals)  ->
        let! envs = get_state
        let signature = Env.lookup p envs.proc
        let calls =
            let f (arg, targ, _) ac = (arg, targ, ac)
            in
                List.map2 f signature.args actuals
        in
            for (arg, targ, ac) in calls do
                let check tactual =               
                    match (ac, TyCmp.compare tactual targ) with
                        (_, 0)                  -> ()
                      | (ByVal _, n) when n > 0 -> Report.Warning.truncated_argument_byval p arg targ tactual
                      | (ByRef _, n) when n > 0 -> Report.Warning.supertyped_argument_byref p arg targ tactual
                      | (_, n)                  -> Report.Warning.subtyped_argument p arg targ tactual
                in
                    match ac with
                        ByVal e ->
                            let! (te, _) = typecheck_expr e
                            check (TyCmp.promote_temporary targ te)

                      | ByRef lv ->
                            let! (tactual, domactual, coerce) = typecheck_lvalue lv
                            let ftarg = Env.lookup arg signature.argenv
                            check tactual;
                            do! coerce ftarg.current
            

  }


and typecheck_expr (e : expr) =
  st {
   match e with
    BinOp (e1, op, e2) ->
        let! (te1, dom1) = typecheck_expr e1
        let! (te2, dom2) = typecheck_expr e2
        let t' =
            match (op, te1, te2) with
                (* unsigned minus *)
                (ArithBinOp Minus, Ty.Numeric (false, n1, d1), Ty.Numeric (false, n2, d2)) -> Ty.TNum (true, max n1 n2, max d1 d2)
                (* plus/minus *)
              | (ArithBinOp (Plus | Minus), Ty.Numeric (sgn1, n1, d1), Ty.Numeric (sgn2, n2, d2)) -> Ty.TNum (sgn1 || sgn2, max n1 n2 + 1us, max d1 d2)
                (* mult *)
              | (ArithBinOp Mult, Ty.Numeric (sgn1, n1, d1), Ty.Numeric (sgn2, n2, d2)) -> Ty.TNum (sgn1 || sgn2, n1 + n2, d1 + d2)
                (* div *)
              | (ArithBinOp Div, Ty.Numeric (sgn1, n1, d1), Ty.Numeric (sgn2, n2, d2)) -> Ty.TNum (sgn1 || sgn2, n1 + d2, d1 + n2)
              
              (* rel ops *)  
              | (RelBinOp _, Ty.Numeric _, Ty.Numeric _)
              | (RelBinOp _, Ty.AlphaNumeric _, Ty.AlphaNumeric _) -> Ty.Bool

              | (RelBinOp _, Ty.AlphaNumeric _, Ty.Numeric _)
              | (RelBinOp _, Ty.Numeric _, Ty.AlphaNumeric _) -> Report.Warning.binop_unsafe_operation te1 te2 op; Ty.Bool

              (* logic ops *)
              | (LogicBinOp _, Ty.Bool, Ty.Bool) -> Ty.Bool

              (* mismatches *)
              | (ArithBinOp _, _, _)  -> Report.binop_mismatch Ty.AnyNumber Ty.AnyNumber te1 te2 op
              | (RelBinOp _, _, _)    -> Report.binop_mismatch Ty.AnyNumber Ty.AnyNumber te1 te2 op
              | (LogicBinOp _, _, _)  -> Report.binop_mismatch Ty.Bool Ty.Bool te1 te2 op

        let dom' =
            match (op, dom1, dom2) with
              | (ArithBinOp op, Dom.Num n1, Dom.Num n2)   -> Dom.Num (Dom.Numeric.reduce_arith (Dom.Numeric.BinOp (n1, op, n2)))
              | (LogicBinOp op, Dom.Bool b1, Dom.Bool b2) -> Dom.Bool (Dom.Boolean.reduce_logic (Dom.Boolean.BinOp (b1, op, b2)))
              | (RelBinOp op, Dom.Num n1, Dom.Num n2)     -> Dom.Bool (Dom.Boolean.reduce_rel n1 n2 op)
              | _                                         -> unexpected "abstract domains in binary application: %s %s %s" (Dom.pretty dom1) (pretty_binop op) (Dom.pretty dom2)

        return (t', dom')
            
  | UnOp (op, e) ->
        let! (te, dom) = typecheck_expr e
        let t' =
            match (op, te) with
                (ArithUnOp Neg, Ty.Numeric (_, n, d)) -> Ty.TNum (true, n, d)
              | (LogicUnOp Not, Ty.Bool)              -> Ty.Bool

              (* mismatches *)
              | (ArithUnOp Neg, _) -> Report.unop_mismatch Ty.AnyNumber te op
              | (LogicUnOp Not, _) -> Report.unop_mismatch Ty.Bool te op
        let dom' =
            match (op, dom) with
              | (ArithUnOp op, Dom.Num n)  -> Dom.Num (Dom.Numeric.reduce_arith (Dom.Numeric.UnOp (op, n)))
              | (LogicUnOp op, Dom.Bool b) -> Dom.Bool (Dom.Boolean.reduce_logic (Dom.Boolean.UnOp (op, b)))
              | _                          -> unexpected "abstract domains in unary application: %s %s" (pretty_unop op) (Dom.pretty dom)
        return (t', dom')
            

  | Lit lit -> return typecheck_lit lit

  | LValue lv ->
        let! (tlv, dom, _) = typecheck_lvalue lv
        return (Ty.Type tlv, dom)
  
  }


and typecheck_lit = function
    Int n -> (Ty.TNum (n < 0L, FormatSize.of_int (sprintf "%d" (Math.Abs n)).Length, 0us), Dom.Num (Dom.Numeric.lit (float n)))

  | Float x ->
        let a = Math.Abs x
        let s = sprintf "%g" a
        let nlen = (sprintf "%g" (Math.Truncate a)).Length
        in
            (Ty.TNum (x < 0.0, FormatSize.of_int nlen, FormatSize.of_int (s.Length - nlen - 1)), Dom.Num (Dom.Numeric.lit x))

  | True  -> (Ty.Bool, Dom.Bool (Dom.Boolean.Const true))
  | False -> (Ty.Bool, Dom.Bool (Dom.Boolean.Const true))

  | String s ->
        let rex = new Regex "[0-9]"
        in
            (Ty.Type ((if rex.Match(s).Success then Ty.AlphaNum else Ty.Alpha) (FormatSize.of_int s.Length)), Dom.Alpha s)



and attempt_typecheck_lvalue choosety lv =
  st {
    let! (t, dom, x, uid, ft, tsubst) =
        let rec f lv =
          st {
           match lv with
            IdAnnot ann ->
                let x = ann.value
                let! venv = get_var_env
                let ft = Env.lookup x venv
                let (t, dom) = choosety x ft
                let uid = ann.unique
                do! lift_topo_env (Env.bind uid ft)
                return (t, dom, x, uid, ft, fun t' -> t')

          | Subscript (lv', e) ->
                let! (tlv, dom, x,  uid, ft, tsubst) = f lv'
                (* TODO: support for array domains *)
                let! (te, dome) = typecheck_expr e
                (* TODO: do some static check with subscript domain 'dome' *)
                return
                    match (tlv, te) with
                        (Ty.Array (ta, n), Ty.Numeric _) ->
                            eval_const_expr e |> Option.iter (fun i -> if i < 1L || i > Convert.ToInt64 n then Report.out_of_bounds tlv i);
                            (ta, not_implemented "array domain", x, uid, ft, fun t' -> tsubst (Ty.Array (t', n)))
                      (* mismatches *)
                      | (Ty.Array (ta, _), _) -> Report.temporary_mismatch Ty.AnyNumber te "array subscript"
                      | _                     -> Report.array_mismatch tlv
                                        
          | Select (_, lb) when lb.ToLower () = "filler" ->
                return Report.filler_in_select lv

          | Select (lv', lb) ->
                let! (tlv, dom, x, uid, ft, tsubst) = f lv'
                (* TODO: support for record domains *)
                return
                    match tlv with
                        Ty.Record bs as r ->
                            match List.tryFind (fun (lb', _, _) -> lb = lb') bs with
                                None             -> Report.undefined_record_label lb r
                              | Some (_, tlb, _) -> (tlb, not_implemented "record domain", x, uid, ft, fun t' -> tsubst (Ty.Record (List.map (function (lb', t, _) when lb' = lb -> (lb, t', None) | rp -> rp) bs)))
                        (* mismatches *)
                        | tlv -> Report.record_expected tlv lb
          }
        in
            f lv
    let coerce (fi : choice_type) =
      st {
        let ft' = ft.coerce (fi.map tsubst) uid        
        do! lift_var_env (Env.bind x ft')
        do! lift_topo_env (Env.bind uid ft')
      }
    return (t, dom, coerce)
  }

and typecheck_lvalue_as_original lv =
  st {
    let choosety x (ft : flow_type) =
        msg Low "lvalue '%s': attempt with original type for %s : %s" (pretty_lvalue pretty_annots_annotated lv) x (ft.pretty);
        (ft.original, ft.domain)
    return! attempt_typecheck_lvalue choosety lv
  }

and typecheck_lvalue_as_current lv =
  st {
    let choosety x (ft : flow_type) =
        let p s = msg Low "lvalue '%s': %s %s : %s" (pretty_lvalue pretty_annots_annotated lv) s x (ft.pretty)
        match ft.current with
            Single t       -> p "attempt with current flow type as single for"; (t, ft.domain)
          | Choice _ as fi -> p "current is a choice in"; Report.ambiguous_choice x fi.pretty
    return! attempt_typecheck_lvalue choosety lv
  }
    
and typecheck_lvalue lv =
  st {
    let p (log : _ logger) s = log High "lvalue '%s' used with its %s type in this context" (pretty_lvalue pretty_annots_annotated lv) s
    try
        let! r = typecheck_lvalue_as_original lv
        p hint "original";
        return r
    with TypeError _ ->
        let! r = typecheck_lvalue_as_current lv
        p warn "current flow";
        return r
  }
