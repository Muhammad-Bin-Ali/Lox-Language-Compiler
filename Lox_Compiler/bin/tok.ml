type token_type =
  (* Single-character tokens *)
  | Left_paren
  | Right_paren
  | Left_brace
  | Right_brace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star

  (* One or two character tokens *)
  | Bang
  | Bang_equal
  | Equal
  | Equal_equal
  | Greater
  | Greater_equal
  | Less
  | Less_equal

  (* Literals *)
  | Identifier
  | String
  | Number

  (* Keywords *)
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While

  (* End of file *)
  | EOF


type token = {
  token_type: token_type;
  lexeme: string;
  literal: string option; (* Use option to allow for None when no literal *)
  line: int;
}

let string_of_token token = 
  let lit_str = match token.literal with
    | Some lit -> lit
    | None -> "None" in
  Printf.sprintf "{ token_type: %s; lexeme: %s; literal: %s; line: %d }"