(* Parse, type-check, then evaluate. Ill-typed programs are rejected before they run. *)

type outcome = { ty : string; value : string }

let error_at (p : Lexer.pos) msg = Printf.sprintf "line %d, column %d: %s" p.line p.col msg

let run src : (outcome, string) result =
  match Parser.parse_program src with
  | exception Lexer.Error (p, msg) -> Error ("lexical error at " ^ error_at p msg)
  | exception Parser.Error (p, msg) -> Error ("syntax error at " ^ error_at p msg)
  | expr -> (
      match Infer.type_of expr with
      | exception Types.Type_error msg -> Error ("type error: " ^ msg)
      | ty -> (
          match Eval.eval Eval.builtins expr with
          | exception Eval.Runtime_error msg -> Error ("runtime error: " ^ msg)
          | v -> Ok { ty = Types.to_string ty; value = Eval.to_string v }))

let type_of src : (string, string) result =
  match Parser.parse_program src with
  | exception Lexer.Error (p, msg) -> Error ("lexical error at " ^ error_at p msg)
  | exception Parser.Error (p, msg) -> Error ("syntax error at " ^ error_at p msg)
  | expr -> (
      match Infer.type_of expr with
      | exception Types.Type_error msg -> Error ("type error: " ^ msg)
      | ty -> Ok (Types.to_string ty))
