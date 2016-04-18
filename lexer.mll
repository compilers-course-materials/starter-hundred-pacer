{
  open Lexing
  open Parser
  open Printf
}

let dec_digit = ['0'-'9']
let signed_int = dec_digit+ | ('-' dec_digit+)

let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*

let blank = [' ' '\t']+
let space = [' ' '\t' '\n']+

rule token = parse
  | blank "(" { LPARENSPACE }
  | '\n' "(" { LPARENSPACE }
  | blank { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | signed_int as x { NUM (int_of_string x) }
  | "main" { MAIN }
  | "def" { DEF }
  | "add1" { ADD1 }
  | "sub1" { SUB1 }
  | "print" { PRINT }
  | "if" { IF }
  | "true" { TRUE }
  | "false" { FALSE }
  | "isbool" { ISBOOL }
  | "isnum" { ISNUM }
  | ":" { COLON }
  | "else:" { ELSECOLON }
  | "let" { LET }
  | "in" { IN }
  | "==" { EQEQ }
  | "=" { EQUAL }
  | "," { COMMA }
  | "(" { LPARENNOSPACE }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "<" { LESS }
  | ">" { GREATER }
  | ident as x { ID x }
  | eof { EOF }
  | _ as c { failwith (sprintf "Unrecognized character: %c" c) }


