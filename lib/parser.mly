%{
    open Syntax
    open Text
%}

%token <int> INT
%token <float> FLOAT
%token <Text.Symbol.t> IDENT
%token LET
%token EQUAL RIGHT_ARROW
%token IN
%token DEF FN
%token DOT
%token LBRACE RBRACE
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACKET_BAR RBRACKET_BAR
%token TRUE FALSE
%token IF THEN ELSE
%token COMMA
%token PLUS MINUS STAR SLASH
%token EQUAL_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL NOT_EQUAL
%token EOF

%type <Syntax.t option> program
%start program
%% 

program
    : expr? EOF { $1 }

simple_expr
    : INT { Uint { value=$1; span=Span.create $sloc } }
    | FLOAT { Ufloat { value=$1; span=Span.create $sloc } }
    | TRUE { Ubool { value=true; span=Span.create $sloc } }
    | FALSE { Ubool { value=false; span=Span.create $sloc } }
    | IDENT { Uvar { value=$1; span=Span.create $sloc } }
    | simple_expr LBRACKET expr RBRACKET { Usubscript { value=$1; index=$3; span=Span.create $sloc } }  
    | simple_expr DOT INT { Utuple_subscript { value=$1; index=$3; span=Span.create $sloc } }
    | simple_expr DOT IDENT { Uselect { value=$1; field=$3; span=Span.create $sloc } }
    | LBRACE record_fields RBRACE { Urecord { fields=$2; span=Span.create $sloc } }
    | LPAREN elements RPAREN { Utuple { values=$2; span=Span.create $sloc } }
    | LBRACKET_BAR elements RBRACKET_BAR { Uvector { values=$2; span=Span.create $sloc } }

expr
    : simple_expr { $1 }
    | simple_expr expr  { Uapp{ fn=$1; value=$2; span=Span.create $sloc } }
    | MINUS expr {
        match $2 with
        | Uint { value; span } -> Uint { value=(-value); span }   
        | _ -> Uneg { value=$2; span=Span.create $sloc }
    }
    | expr PLUS expr { Uop { op=Op.Add; left=$1; right=$3; span=Span.create $sloc } }
    | expr MINUS expr { Uop { op=Op.Sub; left=$1; right=$3; span=Span.create $sloc } }
    | expr STAR expr { Uop { op=Op.Mul; left=$1; right=$3; span=Span.create $sloc } }
    | expr SLASH expr { Uop { op=Op.Div; left=$1; right=$3; span=Span.create $sloc } }
    | expr EQUAL_EQUAL expr { Uop { op=Op.Eq; left=$1; right=$3; span=Span.create $sloc } }
    | expr LESS expr { Uop { op=Op.Lt; left=$1; right=$3; span=Span.create $sloc } }
    | expr GREATER expr { Uop { op=Op.Gt; left=$1; right=$3; span=Span.create $sloc } }
    | expr LESS_EQUAL expr { Uop { op=Op.Leq; left=$1; right=$3; span=Span.create $sloc } }
    | expr GREATER_EQUAL expr { Uop { op=Op.Geq; left=$1; right=$3; span=Span.create $sloc } }
    | expr NOT_EQUAL expr { Uop { op=Op.Neq; left=$1; right=$3; span=Span.create $sloc } }
    | IF expr THEN expr ELSE expr { Uif { cond=$2; then_=$4; else_=$6; span=Span.create $sloc } }
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
    | FN pattern RIGHT_ARROW expr { 
        let lambda = Uclosure { parameter=$2; value=$4; span=Span.create $sloc } in 
        Ulambda { closure=lambda }
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
    : separated_nonempty_list(COMMA, record_field) { $1 }

record_field
    : IDENT EQUAL expr { ($1, $3) }

elements
    : separated_list(COMMA, expr) { $1 }
