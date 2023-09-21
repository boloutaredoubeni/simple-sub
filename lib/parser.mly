%{
    open Syntax
    open Text
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
    : expr? EOF { $1 }

simple_expr
    : INT { Uint { value=$1; span=Span.create $sloc } }
    | IDENT { Uvar { value=$1; span=Span.create $sloc } }
    | simple_expr DOT IDENT { Uselect { value=$1; field=$3; span=Span.create $sloc } }
    | LBRACE record_fields RBRACE { Urecord { fields=$2; span=Span.create $sloc } }

expr
    : simple_expr { $1 }
    | simple_expr expr  { Uapp{ fn=$1; value=$2; span=Span.create $sloc } }
    | LET pattern EQUAL expr IN expr { Ulet { pattern=$2; value=$4; app=$6; span=Span.create $sloc } }
    | LET IDENT pattern RIGHT_ARROW expr IN expr { 
        Ulet_fun { 
            name = $2; 
            closure = Uclosure  { 
                    parameter = $3; 
                    value = $5; 
                    span = Span.create ($startpos($3), $startpos($5)) 
                }; 
                app = $7;
                span = Span.create $sloc  
            }
        }
    | DEF IDENT pattern EQUAL expr IN expr { 
        Udef { 
            name = $2; 
            closure = Uclosure  { 
                    parameter = $3; 
                    value = $5; 
                    span = Span.create ($startpos($3), $startpos($5)) 
                }; 
                app = $7;
                span = Span.create $sloc
            }      
        }

pattern
    : IDENT { Upat_var { value=$1; span=Span.create $sloc } }

record_fields
    : separated_list(COMMA, record_field) { $1 }

record_field
    : IDENT EQUAL expr { ($1, $3) }


