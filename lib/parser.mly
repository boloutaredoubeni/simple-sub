%{
    open Syntax
%}

%token <int> INT
%token <Syntax.Symbol.t> IDENT
%token LET
%token EQUAL RIGHT_ARROW
%token IN
%token DEF
%token DOT
%token LBRACE RBRACE
%token COMMA
%token EOF

%type <Syntax.t option> program
%start program
%%

program
    : EOF { None }
    | expr EOF { Some $1 }

simple_expr
    : INT { Uint $1 }
    | IDENT { Uvar $1 }
    | simple_expr DOT IDENT { Uselect ($1, $3) }
    | LBRACE record_fields RBRACE { Urecord $2 }

expr
    : simple_expr { $1 }
    | simple_expr expr  { Uapp ($1, $2) }
    | LET pattern EQUAL expr IN expr { Ulet ($2, $4, $6) }
    | LET IDENT pattern RIGHT_ARROW expr IN expr { Ulet_fun($2, Uclosure ($3, $5), $7)  }
    | DEF IDENT pattern EQUAL expr IN expr { Udef ($2, Uclosure ($3, $5), $7) }

pattern
    : IDENT { Upat_var $1 }

record_fields
    : separated_list(COMMA, record_field) { $1 }

record_field
    : IDENT EQUAL expr { ($1, $3) }


