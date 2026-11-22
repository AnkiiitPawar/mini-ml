(* Types, unification and let-generalisation (Hindley-Milner with levels).

   Each type variable records the let-nesting "level" at which it was created.
   On leaving a let, variables whose level is deeper than the current one were
   created only inside the let's definition and are generalised. Unification
   lowers levels as variables escape into outer types, so the check is
   near-linear rather than a scan of the whole environment. *)

type ty =
  | TInt
  | TBool
  | TUnit
  | TArrow of ty * ty
  | TPair of ty * ty
  | TList of ty
  | TVar of tvar ref
  | TGen of int  (* a quantified variable inside a type scheme *)

and tvar = Unbound of int * int  (* id, level *) | Link of ty

exception Type_error of string

let counter = ref 0
let current_level = ref 1

let reset () =
  counter := 0;
  current_level := 1

let enter_level () = incr current_level
let leave_level () = decr current_level

let fresh_var () =
  incr counter;
  TVar (ref (Unbound (!counter, !current_level)))

(* Follow links, compressing paths as we go. *)
let rec repr t =
  match t with
  | TVar ({ contents = Link t' } as r) ->
      let t'' = repr t' in
      r := Link t'';
      t''
  | _ -> t

let to_string ty =
  let names = Hashtbl.create 8 in
  let name key =
    match Hashtbl.find_opt names key with
    | Some s -> s
    | None ->
        let k = Hashtbl.length names in
        let s = if k < 26 then Printf.sprintf "'%c" (Char.chr (97 + k)) else Printf.sprintf "'t%d" k in
        Hashtbl.add names key s;
        s
  in
  let paren b s = if b then "(" ^ s ^ ")" else s in
  (* precedence: arrow 1 (right assoc) < pair 2 < list application 3.
     The explicit lets force left-to-right traversal: OCaml evaluates the operands of
     (^) right-to-left, which would hand out 'a, 'b, ... in reverse reading order. *)
  let rec go prec t =
    match repr t with
    | TInt -> "int"
    | TBool -> "bool"
    | TUnit -> "unit"
    | TVar { contents = Unbound (id, _) } -> name (`Var id)
    | TVar { contents = Link _ } -> assert false
    | TGen id -> name (`Gen id)
    | TList a -> go 3 a ^ " list"
    | TPair (a, b) ->
        let left = go 3 a in
        let right = go 3 b in
        paren (prec > 2) (left ^ " * " ^ right)
    | TArrow (a, b) ->
        let left = go 2 a in
        let right = go 1 b in
        paren (prec > 1) (left ^ " -> " ^ right)
  in
  go 0 ty

(* Before binding variable [id] to [t]: fail if [id] occurs in [t] (that would be an
   infinite type such as 'a = 'a -> 'b), and lower the levels of t's variables. *)
let rec occurs_and_adjust id level t =
  match repr t with
  | TVar ({ contents = Unbound (id', level') } as r) ->
      if id = id' then raise (Type_error "this would create an infinite (recursive) type");
      if level' > level then r := Unbound (id', level)
  | TArrow (a, b) | TPair (a, b) ->
      occurs_and_adjust id level a;
      occurs_and_adjust id level b
  | TList a -> occurs_and_adjust id level a
  | TInt | TBool | TUnit | TGen _ | TVar { contents = Link _ } -> ()

let rec unify a b =
  let a = repr a and b = repr b in
  if a == b then ()
  else
    match (a, b) with
    | TVar ({ contents = Unbound (id, level) } as r), t | t, TVar ({ contents = Unbound (id, level) } as r) ->
        occurs_and_adjust id level t;
        r := Link t
    | TArrow (a1, b1), TArrow (a2, b2) | TPair (a1, b1), TPair (a2, b2) ->
        unify a1 a2;
        unify b1 b2
    | TList x, TList y -> unify x y
    | TInt, TInt | TBool, TBool | TUnit, TUnit -> ()
    | _ -> raise (Type_error (Printf.sprintf "cannot unify %s with %s" (to_string a) (to_string b)))

(* Quantify the variables created deeper than the current level. *)
let rec generalize t =
  match repr t with
  | TVar { contents = Unbound (id, level) } when level > !current_level -> TGen id
  | TArrow (a, b) -> TArrow (generalize a, generalize b)
  | TPair (a, b) -> TPair (generalize a, generalize b)
  | TList a -> TList (generalize a)
  | t -> t

(* Replace each quantified variable with a fresh one (consistently within one use). *)
let instantiate t =
  let fresh = Hashtbl.create 8 in
  let rec go t =
    match repr t with
    | TGen id -> (
        match Hashtbl.find_opt fresh id with
        | Some v -> v
        | None ->
            let v = fresh_var () in
            Hashtbl.add fresh id v;
            v)
    | TArrow (a, b) -> TArrow (go a, go b)
    | TPair (a, b) -> TPair (go a, go b)
    | TList a -> TList (go a)
    | t -> t
  in
  go t
