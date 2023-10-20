{
    open Core
    open Parser
    open Text
    open Lexing

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }


}

let space = [' ' '\t']
let newline = ['\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let ident = (lower|'_') (digit|lower|upper|'_'|'$')*
let number = digit+
let decimal = number '.' number

rule token = parse
| space+ { token lexbuf }
| newline { next_line lexbuf; token lexbuf }
| "true" { TRUE }
| "false" { FALSE }
| "let" { LET }
| "in" { IN }
| "def" { DEF }
| "fn" { FN }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "mut" { MUT }
| "&mut" { AS_MUT}
| "ref" { REF }
| "for" { FOR }
| "do"  { DO }
| "to" { TO }
| "downto" { DOWNTO }
| "end" { END }
| "=" { EQUAL }
| "<-" { LEFT_ARROW }
| "->" { RIGHT_ARROW }
| "{" { LBRACE }
| "}" { RBRACE }
| "(" { LPAREN }
| ")" { RPAREN }
| "[|" { LBRACKET_BAR }
| "|]" { RBRACKET_BAR }
| "[" { LBRACKET }
| "]" { RBRACKET }
| ":=" { COLON_EQUAL }
| "!" { BANG }
| ".." { DOT_DOT }
| "." { DOT }
| "," { COMMA }
| "^" { CARET }
| "+" { PLUS }
| "-" { MINUS }
| "*" { STAR }
| "/" { SLASH}
| "+." { PLUS_DOT }
| "-." { MINUS_DOT }
| "*." { STAR_DOT }
| "/." { SLASH_DOT }
| "==" { EQUAL_EQUAL }
| "!=" { NOT_EQUAL }
| "<" { LESS }
| "<=" { LESS_EQUAL }
| ">" { GREATER }
| ">=" { GREATER_EQUAL }
| ";" { SEMICOLON }
| "'" { read_char (None : char option) lexbuf }
| '"' { read_string (Buffer.create 16) lexbuf }
| eof { EOF }
| number { INT (Int.of_string (Lexing.lexeme lexbuf)) }
| decimal { FLOAT (Float.of_string (Lexing.lexeme lexbuf)) }
| ident { IDENT (Symbol.of_string (Lexing.lexeme lexbuf)) }
| _ { 
    let filename = (Lexing.lexeme_start_p lexbuf).pos_fname in
    let filename = if String.is_empty filename then "stdin" else filename in
    raise (Syntax_error { 
        filename; 
        token=Lexing.lexeme lexbuf; 
        position=Position.of_lexing_position(Lexing.lexeme_start_p lexbuf) 
    }) 
}

and read_string buf = parse
| '"' { STRING (Buffer.contents buf) }
| '\\' {
    let ch = escape_char lexbuf in
    Buffer.add_char buf ch;
    read_string buf lexbuf
}
| [^ '"' '\\'] {
    let ch = Lexing.lexeme lexbuf in
    Buffer.add_string buf ch;
    read_string buf lexbuf
}
| _ { 
    let filename = (Lexing.lexeme_start_p lexbuf).pos_fname in
    let filename = if String.is_empty filename then "stdin" else filename in
    raise (Syntax_error { 
        filename; 
        token=""; 
        position=Position.of_lexing_position(Lexing.lexeme_start_p lexbuf) 
    }) }
| eof {
    let filename = (Lexing.lexeme_start_p lexbuf).pos_fname in
    let filename = if String.is_empty filename then "stdin" else filename in
    raise (Syntax_error { 
        filename; 
        token=""; 
        position=Position.of_lexing_position(Lexing.lexeme_start_p lexbuf) 
    }) 
}

and read_char ch = parse
| "'" { 
     let filename = (Lexing.lexeme_start_p lexbuf).pos_fname in
    let filename = if String.is_empty filename then "stdin" else filename in
    match ch with
    | Some ch -> CHAR ch
    | None ->  raise (Syntax_error { 
        filename; 
        token=""; 
        position=Position.of_lexing_position(Lexing.lexeme_start_p lexbuf) 
    })
}
| '\\' {
    let ch = escape_char lexbuf in
    read_char (Some ch) lexbuf
}
| [^ '\'' '\\'] {
    let ch' = Lexing.lexeme lexbuf in
    let filename = (Lexing.lexeme_start_p lexbuf).pos_fname in
    let filename = if String.is_empty filename then "stdin" else filename in
    match ch with
    | Some _ -> raise (Syntax_error { 
        filename; 
        token=ch'; 
        position=Position.of_lexing_position(Lexing.lexeme_start_p lexbuf) 
    })
    | None ->  read_char (Some ch'.[0]) lexbuf
}
| _ { 
    let filename = (Lexing.lexeme_start_p lexbuf).pos_fname in
    let filename = if String.is_empty filename then "stdin" else filename in
    raise (Syntax_error { 
        filename; 
        token=""; 
        position=Position.of_lexing_position(Lexing.lexeme_start_p lexbuf) 
    }) }
| eof { 
    let filename = (Lexing.lexeme_start_p lexbuf).pos_fname in
    let filename = if String.is_empty filename then "stdin" else filename in
    raise (Syntax_error { 
        filename; 
        token=""; 
        position=Position.of_lexing_position(Lexing.lexeme_start_p lexbuf) 
    }) }
and escape_char = parse
| 'n' { '\n' }
| 'r' { '\r' }
| 't' { '\t' }
| '\\' { '\\' }
| '\'' { '\'' }
| '"' { '"' }
| _ { 
    let filename = (Lexing.lexeme_start_p lexbuf).pos_fname in
    let filename = if String.is_empty filename then "stdin" else filename in
    raise (Syntax_error { 
        filename; 
        token=""; 
        position=Position.of_lexing_position(Lexing.lexeme_start_p lexbuf) 
    }) }
| eof { 
    let filename = (Lexing.lexeme_start_p lexbuf).pos_fname in
    let filename = if String.is_empty filename then "stdin" else filename in
    raise (Syntax_error { 
        filename; 
        token=""; 
        position=Position.of_lexing_position(Lexing.lexeme_start_p lexbuf) 
    }) }