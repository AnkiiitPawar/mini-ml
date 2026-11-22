(* mini-ml FILE | mini-ml -e "EXPR" | mini-ml < FILE *)

let read_all ic =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  Buffer.contents buf

let () =
  let src =
    match Array.to_list Sys.argv with
    | [ _; "-e"; expr ] -> expr
    | [ _; path ] ->
        let ic = open_in_bin path in
        let s = read_all ic in
        close_in ic;
        s
    | [ _ ] -> read_all stdin
    | _ ->
        prerr_endline "usage: mini-ml FILE | mini-ml -e EXPR | mini-ml < FILE";
        exit 2
  in
  match Miniml.Toplevel.run src with
  | Ok { ty; value } -> Printf.printf "- : %s = %s\n" ty value
  | Error msg ->
      prerr_endline msg;
      exit 1
