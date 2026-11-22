(* Type inference: Algorithm W, with let-polymorphism via levels (see Types). *)

open Syntax
open Types

let builtins =
  [
    ("fst", TArrow (TPair (TGen (-1), TGen (-2)), TGen (-1)));
    ("snd", TArrow (TPair (TGen (-1), TGen (-2)), TGen (-2)));
    ("not", TArrow (TBool, TBool));
  ]

let rec infer env expr =
  match expr with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Unit -> TUnit
  | Nil -> TList (fresh_var ())
  | Var x -> (
      match List.assoc_opt x env with
      | Some scheme -> instantiate scheme
      | None -> raise (Type_error ("unbound variable " ^ x)))
  | Fun (x, body) ->
      (* a lambda-bound variable is monomorphic inside the body *)
      let tx = fresh_var () in
      TArrow (tx, infer ((x, tx) :: env) body)
  | App (f, arg) ->
      let tf = infer env f in
      let ta = infer env arg in
      let result = fresh_var () in
      unify tf (TArrow (ta, result));
      result
  | Let (x, rhs, body) ->
      enter_level ();
      let t = infer env rhs in
      leave_level ();
      infer ((x, generalize t) :: env) body
  | Let_rec (f, rhs, body) ->
      enter_level ();
      let tf = fresh_var () in
      unify tf (infer ((f, tf) :: env) rhs);
      leave_level ();
      infer ((f, generalize tf) :: env) body
  | If (c, a, b) ->
      unify (infer env c) TBool;
      let ta = infer env a in
      unify ta (infer env b);
      ta
  | Pair (a, b) ->
      let ta = infer env a in
      TPair (ta, infer env b)
  | Binop (op, a, b) -> (
      let ta = infer env a in
      let tb = infer env b in
      match op with
      | Add | Sub | Mul | Div | Mod ->
          unify ta TInt;
          unify tb TInt;
          TInt
      | Lt | Le | Gt | Ge ->
          unify ta TInt;
          unify tb TInt;
          TBool
      | Eq | Neq ->
          unify ta tb;
          TBool
      | And | Or ->
          unify ta TBool;
          unify tb TBool;
          TBool
      | Cons ->
          unify tb (TList ta);
          tb)
  | Match_list (scrutinee, on_nil, head, tail, on_cons) ->
      let elem = fresh_var () in
      unify (infer env scrutinee) (TList elem);
      let t_nil = infer env on_nil in
      unify t_nil (infer ((tail, TList elem) :: (head, elem) :: env) on_cons);
      t_nil

(* Principal type of a closed expression, as a string such as "('a -> 'b) -> 'a list -> 'b list". *)
let type_of expr =
  reset ();
  enter_level ();
  let t = infer builtins expr in
  leave_level ();
  generalize t
