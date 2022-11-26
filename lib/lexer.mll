{
    open Parser
    open Syntax
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let ident = (lower|'_') (digit|lower|upper|'_'|'$'|'\'')*
let number = digit+

rule token = parse
| space+ { token lexbuf }
| "let" { LET }
| "in" { IN }
| "def" { DEF }
| "=" { EQUAL }
| "->" { RIGHT_ARROW }
| "{" { LBRACE }
| "}" { RBRACE }
| "." { DOT }
| number { INT (int_of_string (Lexing.lexeme lexbuf)) }
| ident { IDENT (Symbol.of_string (Lexing.lexeme lexbuf)) }
| _
    { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }