{
    open Core
    open Parser
    open Text

    exception SyntaxError of { filename: string; token: string; position: Position.t }
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let ident = (lower|'_') (digit|lower|upper|'_'|'$'|'\'')*
let number = digit+
let decimal = number '.' number

rule token = parse
| space+ { token lexbuf }
| "true" { TRUE }
| "false" { FALSE }
| "let" { LET }
| "in" { IN }
| "def" { DEF }
| "fn" { FN }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "=" { EQUAL }
| "->" { RIGHT_ARROW }
| "{" { LBRACE }
| "}" { RBRACE }
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "." { DOT }
| "," { COMMA }
| "+" { PLUS }
| "-" { MINUS }
| "*" { STAR }
| "/" { SLASH}
| "==" { EQUAL_EQUAL }
| "!=" { NOT_EQUAL }
| "<" { LESS }
| "<=" { LESS_EQUAL }
| ">" { GREATER }
| ">=" { GREATER_EQUAL }
| eof { EOF }
| number { INT (Int.of_string (Lexing.lexeme lexbuf)) }
| decimal { FLOAT (Float.of_string (Lexing.lexeme lexbuf)) }
| ident { IDENT (Symbol.of_string (Lexing.lexeme lexbuf)) }
| _ { raise (SyntaxError { 
        filename=(Lexing.lexeme_start_p lexbuf).pos_fname; 
        token=Lexing.lexeme lexbuf; 
        position=Position.of_lexing_position(Lexing.lexeme_start_p lexbuf) 
    }) 
}