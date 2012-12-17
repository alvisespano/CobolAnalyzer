(*
 * COBOL Type Inference
 * Prelude.fs: misc utils
 * (C) 2010 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)
 
#light

module CobolAnalyzer.Engine.Prelude

open System
open System.Collections.Generic
open System.Text.RegularExpressions

exception Unexpected of string

let identity x = x

let throw_formatted exn fmt = Printf.kprintf (fun s -> raise (exn s)) fmt

let unexpected fmt = throw_formatted Unexpected fmt

let not_implemented fmt = throw_formatted (fun s -> new NotImplementedException(s)) fmt

let equal_float (x : float) y = Math.Abs (x - y) <= Double.Epsilon

let flatten_strings sep sc =
    match Seq.length sc with
        0 -> ""
      | 1 -> Seq.head sc
      | _ -> Seq.fold (fun r s -> r + sep + s) (Seq.head sc) (Seq.skip 1 sc)

let mappen_strings f sep c = flatten_strings sep (Seq.map f c)

let separate2 f = List.fold (fun (aa, bb) x -> match f x with Choice1Of2 a -> (a :: aa, bb) | Choice2Of2 b -> (aa, b :: bb)) ([], [])

let fresh =
    let cnt = ref 0
    in
        fun () -> let r = !cnt in incr cnt; r

let something f def = function
    None   -> def
  | Some x -> f x

let some_string f = something f ""

let either def o = something (fun x -> x) def o

let split_string_on_size n (s : string) =
    let m = s.Length % n
    in
        Array.init (s.Length / n + (if m > 0 then 1 else 0)) (fun i -> s.Substring (i * n, if i * n + n > s.Length then m else n))


(* log line priority type *)

[<CustomComparison;CustomEquality>]
type pri = Unmaskerable | High | Normal | Low | Min
with
    static member Parse = function
        "U" -> Unmaskerable
      | "H" -> High
      | "N" -> Normal
      | "L" -> Low
      | "M" -> Min
      | s   -> invalidArg "s" (sprintf "invalid string '%s'" s)

    static member op_Explicit p =
        match p with
            Unmaskerable    -> 4
          | High            -> 3
          | Normal          -> 2
          | Low             -> 1
          | Min             -> 0

    interface IComparable with
        member self.CompareTo p = compare (int self) (int (p :?> pri))

    override self.Equals p = (self :> IComparable).CompareTo p = 0
    override self.GetHashCode () = (int self).GetHashCode ()

    member self.pretty =
        match self with
            Unmaskerable    -> "U"
          | High            -> "H"
          | Normal          -> "N"
          | Low             -> "L"
          | Min             -> "M"


(* state monad *)

module StateMonad =

    type builder () =
        member m.Delay f = f ()
        member m.Return x = fun s -> (x, s)
        member m.Bind (e, f) = fun s -> let (r, s') = e s in f r s'
        member m.Zero () = fun s -> ((), s)
        member m.For (seq, f) = fun s -> ((), Seq.fold (fun s x -> let ((), s') = f x s in s') s seq)
        member m.TryWith (e : 's -> 'a * 's, catch) = fun s -> try e s with exn -> (printf "CAZZA\n"; catch exn s)
        member m.ReturnFrom f = fun s -> f s

    let st = new builder ()
    let get_state = fun s -> (s, s)
    let set_state s = fun _ -> ((), s)

    let lift f x = fun s -> (f x, s)

    let some_state f = function
        None   -> fun s -> ((), s)
      | Some x -> f x


    (*module Test =    

        type t = { A : int; B : string }

        let get_a env = env.A, env
        let set_a a' t = (), { t with A = a' }
        let get_b env = env.B, env
        let set_b b' t = (), { t with B = b' }

        let foo =
          st {
            try
                do! fun () -> (raise (Failure "boo")), ()
                return 1
            with Failure _ -> return 0
          }


        let foo1 =    
            st.TryWith(st.Bind((fun () -> raise (Failure "boo"), ()), fun () -> st.Return 1), (fun e -> st.Return 0))*)

