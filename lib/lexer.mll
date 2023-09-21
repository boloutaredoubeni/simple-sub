{
    open Parser
    open Syntax
    open Text

    exception SyntaxError of { filename: string; token: string; position: Position.t }
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
| eof { EOF }
| number { INT (int_of_string (Lexing.lexeme lexbuf)) }
| ident { IDENT (Symbol.of_string (Lexing.lexeme lexbuf)) }
| _ { raise (SyntaxError { 
        filename=(Lexing.lexeme_start_p lexbuf).pos_fname; 
        token=Lexing.lexeme lexbuf; 
        position=Position.of_lexing_position(Lexing.lexeme_start_p lexbuf) 
    }) 
}