(* Environment-based evaluator. Closures capture their environment; a recursive
   closure's environment is a reference that is patched to contain the closure. *)

open Syntax

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VPair of value * value
  | VList of value list
  | VClosure of string * expr * env ref
  | VBuiltin of (value -> value)

and env = (string * value) list

exception Runtime_error of string

let fail msg = raise (Runtime_error msg)

let rec equal a b =
  match (a, b) with
  | VInt x, VInt y -> x = y
  | VBool x, VBool y -> x = y
  | VUnit, VUnit -> true
  | VPair (a1, b1), VPair (a2, b2) -> equal a1 a2 && equal b1 b2
  | VList xs, VList ys -> List.length xs = List.length ys && List.for_all2 equal xs ys
  | (VClosure _ | VBuiltin _), _ | _, (VClosure _ | VBuiltin _) -> fail "functions cannot be compared"
  | _ -> fail "comparison of values of different types"

let builtins =
  [
    ("fst", VBuiltin (function VPair (a, _) -> a | _ -> fail "fst expects a pair"));
    ("snd", VBuiltin (function VPair (_, b) -> b | _ -> fail "snd expects a pair"));
    ("not", VBuiltin (function VBool b -> VBool (not b) | _ -> fail "not expects a bool"));
  ]

let int_of = function VInt n -> n | _ -> fail "expected an int"
let bool_of = function VBool b -> b | _ -> fail "expected a bool"

let rec eval env expr =
  match expr with
  | Int n -> VInt n
  | Bool b -> VBool b
  | Unit -> VUnit
  | Nil -> VList []
  | Var x -> ( match List.assoc_opt x env with Some v -> v | None -> fail ("unbound variable " ^ x))
  | Fun (x, body) -> VClosure (x, body, ref env)
  | App (f, arg) ->
      let fv = eval env f in
      apply fv (eval env arg)
  | Let (x, rhs, body) -> eval ((x, eval env rhs) :: env) body
  | Let_rec (f, Fun (x, fbody), body) ->
      let closure_env = ref env in
      let closure = VClosure (x, fbody, closure_env) in
      closure_env := (f, closure) :: env;
      eval ((f, closure) :: env) body
  | Let_rec _ -> fail "let rec must define a function"
  | If (c, a, b) -> if bool_of (eval env c) then eval env a else eval env b
  | Pair (a, b) ->
      let va = eval env a in
      VPair (va, eval env b)
  | Binop (And, a, b) -> if bool_of (eval env a) then eval env b else VBool false
  | Binop (Or, a, b) -> if bool_of (eval env a) then VBool true else eval env b
  | Binop (op, a, b) -> (
      let va = eval env a in
      let vb = eval env b in
      match op with
      | Add -> VInt (int_of va + int_of vb)
      | Sub -> VInt (int_of va - int_of vb)
      | Mul -> VInt (int_of va * int_of vb)
      | Div -> if int_of vb = 0 then fail "division by zero" else VInt (int_of va / int_of vb)
      | Mod -> if int_of vb = 0 then fail "division by zero" else VInt (int_of va mod int_of vb)
      | Lt -> VBool (int_of va < int_of vb)
      | Le -> VBool (int_of va <= int_of vb)
      | Gt -> VBool (int_of va > int_of vb)
      | Ge -> VBool (int_of va >= int_of vb)
      | Eq -> VBool (equal va vb)
      | Neq -> VBool (not (equal va vb))
      | Cons -> ( match vb with VList xs -> VList (va :: xs) | _ -> fail "(::) expects a list")
      | And | Or -> assert false)
  | Match_list (scrutinee, on_nil, head, tail, on_cons) -> (
      match eval env scrutinee with
      | VList [] -> eval env on_nil
      | VList (x :: xs) -> eval ((tail, VList xs) :: (head, x) :: env) on_cons
      | _ -> fail "match expects a list")

and apply f arg =
  match f with
  | VClosure (x, body, closure_env) -> eval ((x, arg) :: !closure_env) body
  | VBuiltin fn -> fn arg
  | _ -> fail "applying something that is not a function"

let rec to_string = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VUnit -> "()"
  | VPair (a, b) -> "(" ^ to_string a ^ ", " ^ to_string b ^ ")"
  | VList xs -> "[" ^ String.concat "; " (List.map to_string xs) ^ "]"
  | VClosure _ | VBuiltin _ -> "<fun>"
