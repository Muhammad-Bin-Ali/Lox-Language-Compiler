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

let string_of_token_type = function
  | Left_paren -> "Left_paren"
  | Right_paren -> "Right_paren"
  | Left_brace -> "Left_brace"
  | Right_brace -> "Right_brace"
  | Comma -> "Comma"
  | Dot -> "Dot"
  | Minus -> "Minus"
  | Plus -> "Plus"
  | Semicolon -> "Semicolon"
  | Slash -> "Slash"
  | Star -> "Star"
  | Bang -> "Bang"
  | Bang_equal -> "Bang_equal"
  | Equal -> "Equal"
  | Equal_equal -> "Equal_equal"
  | Greater -> "Greater"
  | Greater_equal -> "Greater_equal"
  | Less -> "Less"
  | Less_equal -> "Less_equal"
  | Identifier -> "Identifier"
  | String -> "String"
  | Number -> "Number"
  | And -> "And"
  | Class -> "Class"
  | Else -> "Else"
  | False -> "False"
  | Fun -> "Fun"
  | For -> "For"
  | If -> "If"
  | Nil -> "Nil"
  | Or -> "Or"
  | Print -> "Print"
  | Return -> "Return"
  | Super -> "Super"
  | This -> "This"
  | True -> "True"
  | Var -> "Var"
  | While -> "While"
  | EOF -> "EOF"

type t = {
  token_type: token_type;
  lexeme: string;
  literal: string option; (* Use option to allow for None when no literal *)
  line: int;
}

let string_of_token token = 
  let lit_str = match token.literal with
    | Some lit -> lit
    | None -> "None" in
  Printf.sprintf "{ token_type: %s; lexeme: %s; literal: %s; line: %d }" (string_of_token_type token.token_type)
    token.lexeme lit_str token.line