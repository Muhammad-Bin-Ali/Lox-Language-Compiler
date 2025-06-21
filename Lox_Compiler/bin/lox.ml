let read_file filename = 
  let ic = open_in filename in
  let try_read () = try Some(input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with 
    | Some line -> loop (line::acc)
    | None -> close_in ic; List.rev acc in 
  let lines = loop [] in
  String.concat "" lines

let report line where message = 
  Printf.eprintf "[line %d] Error%s: %s\n" line where message
  
let error line message = report line "" message
  
let run code = 
  let tokens = Scanner.scan_tokens code in
  List.iter (fun token -> print_endline (Token.string_of_token token)) tokens

let run_prompt () = 
  let try_read_line () = try Some(read_line ()) with End_of_file -> None in
  let rec loop () =
    let line = try_read_line () in
    match line with 
    | Some line -> run line; loop ()
    | None -> print_endline "Exiting Lox interpreter."; exit 0
  in
  loop ()


let run_file filename = 
  let code = read_file filename in 
  run code

let () = 
  match Array.length Sys.argv with 
  | 1 -> run_prompt ()
  | 2 -> run_file Sys.argv.(1)
  | _ -> print_endline "Usage: jlox [script]"

