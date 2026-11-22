(* Hand-written lexer with line/column tracking and nested (* comments *). *)

type token =
  | INT of int
  | IDENT of string
  | TRUE | FALSE | LET | REC | IN | FUN | IF | THEN | ELSE | MATCH | WITH | MOD
  | LPAREN | RPAREN | LBRACKET | RBRACKET | COMMA | SEMI | SEMISEMI | ARROW | BAR | CONS
  | PLUS | MINUS | STAR | SLASH | EQ | NEQ | LT | LE | GT | GE | ANDAND | OROR
  | EOF

type pos = { line : int; col : int }

exception Error of pos * string

let keywords =
  [
    ("true", TRUE); ("false", FALSE); ("let", LET); ("rec", REC); ("in", IN); ("fun", FUN);
    ("if", IF); ("then", THEN); ("else", ELSE); ("match", MATCH); ("with", WITH); ("mod", MOD);
  ]

let symbols2 =
  [ ("->", ARROW); ("::", CONS); (";;", SEMISEMI); ("<>", NEQ); ("<=", LE); (">=", GE); ("&&", ANDAND); ("||", OROR) ]

let symbols1 =
  [
    ('(', LPAREN); (')', RPAREN); ('[', LBRACKET); (']', RBRACKET); (',', COMMA); (';', SEMI);
    ('|', BAR); ('+', PLUS); ('-', MINUS); ('*', STAR); ('/', SLASH); ('=', EQ); ('<', LT); ('>', GT);
  ]

let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_ident_char c = is_ident_start c || (c >= '0' && c <= '9') || c = '\''
let is_digit c = c >= '0' && c <= '9'

let tokenize (src : string) : (token * pos) array =
  let n = String.length src in
  let toks = ref [] in
  let i = ref 0 and line = ref 1 and col = ref 1 in
  let here () = { line = !line; col = !col } in
  let bump () =
    if src.[!i] = '\n' then (incr line; col := 1) else incr col;
    incr i
  in
  let peek k = if !i + k < n then Some src.[!i + k] else None in
  while !i < n do
    let c = src.[!i] in
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then bump ()
    else if c = '(' && peek 1 = Some '*' then begin
      let start = here () in
      bump (); bump ();
      let depth = ref 1 in
      while !depth > 0 do
        if !i >= n then raise (Error (start, "unterminated comment"));
        if src.[!i] = '(' && peek 1 = Some '*' then (bump (); bump (); incr depth)
        else if src.[!i] = '*' && peek 1 = Some ')' then (bump (); bump (); decr depth)
        else bump ()
      done
    end
    else begin
      let p = here () in
      if is_digit c then begin
        let start = !i in
        while !i < n && is_digit src.[!i] do bump () done;
        let text = String.sub src start (!i - start) in
        match int_of_string_opt text with
        | Some v -> toks := (INT v, p) :: !toks
        | None -> raise (Error (p, "integer literal too large: " ^ text))
      end
      else if is_ident_start c then begin
        let start = !i in
        while !i < n && is_ident_char src.[!i] do bump () done;
        let word = String.sub src start (!i - start) in
        let tok = match List.assoc_opt word keywords with Some k -> k | None -> IDENT word in
        toks := (tok, p) :: !toks
      end
      else
        let two = if !i + 1 < n then String.sub src !i 2 else "" in
        match List.assoc_opt two symbols2 with
        | Some tok -> bump (); bump (); toks := (tok, p) :: !toks
        | None -> (
            match List.assoc_opt c symbols1 with
            | Some tok -> bump (); toks := (tok, p) :: !toks
            | None -> raise (Error (p, Printf.sprintf "unexpected character '%c'" c)))
    end
  done;
  Array.of_list (List.rev ((EOF, here ()) :: !toks))

let describe = function
  | INT n -> string_of_int n
  | IDENT s -> s
  | TRUE -> "true" | FALSE -> "false" | LET -> "let" | REC -> "rec" | IN -> "in" | FUN -> "fun"
  | IF -> "if" | THEN -> "then" | ELSE -> "else" | MATCH -> "match" | WITH -> "with" | MOD -> "mod"
  | LPAREN -> "(" | RPAREN -> ")" | LBRACKET -> "[" | RBRACKET -> "]" | COMMA -> "," | SEMI -> ";"
  | SEMISEMI -> ";;" | ARROW -> "->" | BAR -> "|" | CONS -> "::" | PLUS -> "+" | MINUS -> "-"
  | STAR -> "*" | SLASH -> "/" | EQ -> "=" | NEQ -> "<>" | LT -> "<" | LE -> "<=" | GT -> ">"
  | GE -> ">=" | ANDAND -> "&&" | OROR -> "||" | EOF -> "end of input"
