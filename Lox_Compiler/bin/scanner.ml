type t = {source: string; tokens: Token.t list}

let scan_tokens source =
  let rec scan_next acc text = match text with
    | "" -> List.rev acc
    | _ ->
      let token, rest = Token.scan text in
      scan_next (token :: acc) rest
  in
  scan_next [] source