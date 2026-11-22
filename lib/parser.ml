(* Recursive-descent parser with precedence climbing for binary operators.

   program ::= { "let" ["rec"] IDENT {IDENT} "=" expr ";;" } [ expr ] EOF
   expr    ::= "let" ["rec"] IDENT {IDENT} "=" expr "in" expr
             | "fun" IDENT {IDENT} "->" expr
             | "if" expr "then" expr "else" expr
             | "match" expr "with" ["|"] "[" "]" "->" expr "|" IDENT "::" IDENT "->" expr
             | binary
   binary  ::= operators, loosest first: ||  &&  ( = <> < <= > >= )  ::  ( + - )  ( * / mod )
   app     ::= ["-"] atom { atom }
   atom    ::= INT | "true" | "false" | IDENT | "()" | "(" expr ["," expr] ")"
             | "[" [ expr { ";" expr } ] "]" *)

open Syntax
open Lexer

exception Error of pos * string

type state = { toks : (token * pos) array; mutable i : int }

let peek st = fst st.toks.(st.i)
let pos st = snd st.toks.(st.i)
let advance st = st.i <- st.i + 1
let fail st msg = raise (Error (pos st, msg))

let expect st tok =
  if peek st = tok then advance st
  else fail st (Printf.sprintf "expected '%s' but found '%s'" (describe tok) (describe (peek st)))

let ident st =
  match peek st with
  | IDENT x -> advance st; x
  | t -> fail st (Printf.sprintf "expected a name but found '%s'" (describe t))

let rec params st = match peek st with IDENT _ -> let x = ident st in x :: params st | _ -> []
let lambda xs body = List.fold_right (fun x acc -> Fun (x, acc)) xs body

(* binding precedence, right-associativity, operator *)
let binop_info = function
  | OROR -> Some (1, true, Or)
  | ANDAND -> Some (2, true, And)
  | EQ -> Some (3, false, Eq) | NEQ -> Some (3, false, Neq)
  | LT -> Some (3, false, Lt) | LE -> Some (3, false, Le)
  | GT -> Some (3, false, Gt) | GE -> Some (3, false, Ge)
  | CONS -> Some (4, true, Cons)
  | PLUS -> Some (5, false, Add) | MINUS -> Some (5, false, Sub)
  | STAR -> Some (6, false, Mul) | SLASH -> Some (6, false, Div) | MOD -> Some (6, false, Mod)
  | _ -> None

let starts_atom = function INT _ | IDENT _ | TRUE | FALSE | LPAREN | LBRACKET -> true | _ -> false

(* "let" ["rec"] name params "=" rhs: returns (is_rec, name, bound expression) *)
let rec binding st =
  expect st LET;
  let is_rec = if peek st = REC then (advance st; true) else false in
  let name = ident st in
  let ps = params st in
  if is_rec && ps = [] then fail st "'let rec' must define a function";
  expect st EQ;
  let rhs = expr st in
  (is_rec, name, lambda ps rhs)

and expr st =
  match peek st with
  | LET ->
      let is_rec, name, rhs = binding st in
      expect st IN;
      let body = expr st in
      if is_rec then Let_rec (name, rhs, body) else Let (name, rhs, body)
  | FUN ->
      advance st;
      let ps = params st in
      if ps = [] then fail st "'fun' needs at least one parameter";
      expect st ARROW;
      lambda ps (expr st)
  | IF ->
      advance st;
      let c = expr st in
      expect st THEN;
      let a = expr st in
      expect st ELSE;
      If (c, a, expr st)
  | MATCH ->
      advance st;
      let scrutinee = expr st in
      expect st WITH;
      if peek st = BAR then advance st;
      expect st LBRACKET;
      expect st RBRACKET;
      expect st ARROW;
      let on_nil = expr st in
      expect st BAR;
      let head = ident st in
      expect st CONS;
      let tail = ident st in
      expect st ARROW;
      Match_list (scrutinee, on_nil, head, tail, expr st)
  | _ -> binary st 0

and binary st min_prec =
  let lhs = ref (app st) in
  let rec loop () =
    match binop_info (peek st) with
    | Some (prec, right_assoc, op) when prec >= min_prec ->
        advance st;
        let rhs = binary st (if right_assoc then prec else prec + 1) in
        lhs := Binop (op, !lhs, rhs);
        loop ()
    | _ -> ()
  in
  loop ();
  !lhs

and app st =
  if peek st = MINUS then (advance st; Binop (Sub, Int 0, app st))
  else begin
    let f = ref (atom st) in
    while starts_atom (peek st) do
      f := App (!f, atom st)
    done;
    !f
  end

and atom st =
  match peek st with
  | INT n -> advance st; Int n
  | TRUE -> advance st; Bool true
  | FALSE -> advance st; Bool false
  | IDENT x -> advance st; Var x
  | LPAREN ->
      advance st;
      if peek st = RPAREN then (advance st; Unit)
      else
        let e = expr st in
        if peek st = COMMA then begin
          advance st;
          let e2 = expr st in
          expect st RPAREN;
          Pair (e, e2)
        end
        else (expect st RPAREN; e)
  | LBRACKET ->
      advance st;
      if peek st = RBRACKET then (advance st; Nil)
      else begin
        let items = ref [ expr st ] in
        while peek st = SEMI do
          advance st;
          items := expr st :: !items
        done;
        expect st RBRACKET;
        List.fold_left (fun acc e -> Binop (Cons, e, acc)) Nil !items
      end
  | t -> fail st (Printf.sprintf "expected an expression but found '%s'" (describe t))

let parse_expr src =
  let st = { toks = tokenize src; i = 0 } in
  let e = expr st in
  if peek st <> EOF then fail st (Printf.sprintf "unexpected '%s' after expression" (describe (peek st)));
  e

(* A program is a series of top-level "let ... ;;" definitions and an optional final
   expression. It is desugared into nested lets; with no final expression the value of
   the last definition is the result. *)
let parse_program src =
  let st = { toks = tokenize src; i = 0 } in
  let rec go last =
    if peek st = LET then begin
      let save = st.i in
      let is_rec, name, rhs = binding st in
      if peek st = IN then begin
        (* not a top-level definition after all: an ordinary let-expression *)
        st.i <- save;
        let e = expr st in
        if peek st = SEMISEMI then advance st;
        if peek st <> EOF then fail st "unexpected input after the final expression";
        e
      end
      else begin
        if peek st = SEMISEMI then advance st;
        let body = go (Some name) in
        if is_rec then Let_rec (name, rhs, body) else Let (name, rhs, body)
      end
    end
    else if peek st = EOF then
      match last with Some name -> Var name | None -> fail st "empty program"
    else begin
      let e = expr st in
      if peek st = SEMISEMI then advance st;
      if peek st <> EOF then fail st (Printf.sprintf "unexpected '%s'" (describe (peek st)));
      e
    end
  in
  go None
