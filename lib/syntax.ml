(* Abstract syntax of mini-ML. *)

type binop = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Le | Gt | Ge | And | Or | Cons

type expr =
  | Int of int
  | Bool of bool
  | Unit
  | Nil
  | Var of string
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Let_rec of string * expr * expr  (* the bound expression is always a Fun *)
  | If of expr * expr * expr
  | Binop of binop * expr * expr
  | Pair of expr * expr
  | Match_list of expr * expr * string * string * expr
      (* match e with [] -> e_nil | head :: tail -> e_cons *)
