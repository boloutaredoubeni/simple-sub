{
    open Parser
    open Syntax

    exception SyntaxError of { token: string; start: int; end_: int }
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
| "," { COMMA }
| number { INT (int_of_string (Lexing.lexeme lexbuf)) }
| ident { IDENT (Symbol.of_string (Lexing.lexeme lexbuf)) }
| _ { raise (SyntaxError { token=(Lexing.lexeme lexbuf);
                           start=(Lexing.lexeme_start lexbuf);
                           end_=(Lexing.lexeme_end lexbuf) }) }