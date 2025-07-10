{
    open Lexing
    open Parser
}

let num = ['0'-'9']
let symbol = ['a'-'z' 'A'-'Z' '_' '-' '+' '*' '=' '?' ':' '!']

rule read = parse
    | [' ' '\t' '\n']+                { read lexbuf }
    | "--"                            { comment lexbuf }
    | "("                             { LPAREN }
    | ")"                             { RPAREN }
    | "lazy"                          { LAZY }
    | "let"                           { LET }
    | "="                             { EQUALS }
    | "in"                            { IN }
    | "fix"                           { FIX }
    | "and"                           { AND }
    | "if"                            { IF }
    | "then"                          { THEN }
    | "else"                          { ELSE }
    | "\\"                            { LAMBDA }
    | "->"                            { ARROW }
    | num+                            { INT (int_of_string (lexeme lexbuf)) }
    | "()"                            { UNIT () }
    | "true" | "false"                { BOOL (bool_of_string (lexeme lexbuf)) }
    | (symbol (num | symbol)*)        { SYMBOL (lexeme lexbuf) }
    | eof                             { EOF }

  and comment = parse
    | [^ '\n']* '\n'        { read lexbuf }
    | eof                   { EOF }
