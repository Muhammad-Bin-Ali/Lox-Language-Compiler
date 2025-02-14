let read_file filename = 
  let ic = open_in filename in
  let try_read () = try Some(input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with 
    | Some line -> loop (line::acc)
    | None -> close_in ic; List.rev acc in 
  let lines = loop [] in
  String.concat "" lines

let run code = ()

let run_prompt () = 
  let rec loop () =
    let line = read_line () in
    run line
    loop () in
  loop ()


let run_file filename = 
  let code = read_file filename in 
  run code

let () = 
  match Array.length Sys.argv with 
  | 1 -> run_prompt ()
  | 2 -> run_file Sys.argv.(1)
  | _ -> print_endline "Usage: jlox [script]"

