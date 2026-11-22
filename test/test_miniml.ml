open Miniml

let failures = ref 0

let report name ok detail =
  Printf.printf "%s %s%s\n" (if ok then "PASS" else "FAIL") name (if ok then "" else "  -- " ^ detail);
  if not ok then incr failures

let type_is src expected =
  match Toplevel.type_of src with
  | Ok t -> report ("type of " ^ src) (t = expected) ("got " ^ t)
  | Error e -> report ("type of " ^ src) false e

let runs_to src ty value =
  match Toplevel.run src with
  | Ok o -> report ("value of " ^ String.escaped src) (o.ty = ty && o.value = value) (o.ty ^ " = " ^ o.value)
  | Error e -> report ("value of " ^ String.escaped src) false e

let rejected src prefix =
  let starts s = String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix in
  match Toplevel.run src with
  | Ok o -> report ("rejects " ^ src) false ("accepted with type " ^ o.ty)
  | Error e -> report ("rejects " ^ src) (starts e) e

let map_src = "let rec map f xs = match xs with [] -> [] | x :: rest -> f x :: map f rest in "

let () =
  (* principal types *)
  type_is "fun x -> x" "'a -> 'a";
  type_is "fun x y -> x" "'a -> 'b -> 'a";
  type_is "fun f -> fun x -> f (f x)" "('a -> 'a) -> 'a -> 'a";
  type_is "fun f g x -> f (g x)" "('a -> 'b) -> ('c -> 'a) -> 'c -> 'b";
  type_is "fun p -> (snd p, fst p)" "'a * 'b -> 'b * 'a";
  type_is (map_src ^ "map") "('a -> 'b) -> 'a list -> 'b list";
  type_is "let rec fold f acc xs = match xs with [] -> acc | x :: r -> fold f (f acc x) r in fold"
    "('a -> 'b -> 'a) -> 'a -> 'b list -> 'a";
  type_is "[[1; 2]; []]" "int list list";
  type_is "fun x -> (x, [x])" "'a -> 'a * 'a list";
  (* let-polymorphism: id is used at two types *)
  type_is "let id = fun x -> x in (id 1, id true)" "int * bool";
  (* ...but a lambda-bound variable is monomorphic *)
  rejected "fun f -> (f 1, f true)" "type error";
  rejected "fun x -> x x" "type error: this would create an infinite";
  rejected "1 + true" "type error";
  rejected "if 1 then 2 else 3" "type error";
  rejected "[1; true]" "type error";
  rejected "y + 1" "type error: unbound variable y"

let () =
  (* evaluation *)
  runs_to "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 10" "int" "3628800";
  runs_to "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 20" "int" "6765";
  runs_to (map_src ^ "map (fun x -> x * x) [1; 2; 3; 4]") "int list" "[1; 4; 9; 16]";
  runs_to (map_src ^ "map (fun x -> x > 2) [1; 2; 3]") "bool list" "[false; false; true]";
  runs_to "let compose f g x = f (g x) in compose (fun x -> x + 1) (fun x -> x * 2) 5" "int" "11";
  runs_to "let pair = (1, (true, [3])) in fst (snd pair)" "bool" "true";
  runs_to "[1; 2] = [1; 2] && not ([1] = [2])" "bool" "true";
  runs_to "17 mod 5 - -3" "int" "5";
  runs_to "let rec length xs = match xs with [] -> 0 | _x :: r -> 1 + length r;;\n\
           let rec range a b = if a > b then [] else a :: range (a + 1) b;;\n\
           length (range 1 1000)" "int" "1000";
  runs_to "let x = 5;; let y = x * 2;;" "int" "10";
  runs_to "(* comments (* nest *) *) 1 + 2" "int" "3"

let () =
  (* errors are reported with a position or a clear cause *)
  rejected "let x = in 3" "syntax error at line 1, column 9";
  rejected "1 +\n  $" "lexical error at line 2, column 3";
  rejected "10 / (5 - 5)" "runtime error: division by zero";
  rejected "let f = fun x -> x in f = f" "runtime error: functions cannot be compared"

let () =
  if !failures > 0 then (
    Printf.printf "%d failure(s)\n" !failures;
    exit 1)
  else print_endline "all tests passed"
