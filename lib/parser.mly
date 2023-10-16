%{
    open Syntax
    open Text
%}

%token <int> INT
%token <float> FLOAT
%token <Text.Symbol.t> IDENT
%token LET MUT
%token EQUAL 
%token LEFT_ARROW RIGHT_ARROW
%token IN
%token DEF FN
%token DOT
%token TO DOWNTO
%token LBRACE RBRACE
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACKET_BAR RBRACKET_BAR
%token TRUE FALSE
%token FOR DO END
%token IF THEN ELSE
%token COMMA
%token SEMICOLON
%token PLUS MINUS STAR SLASH
%token PLUS_DOT MINUS_DOT STAR_DOT SLASH_DOT
%token EQUAL_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL NOT_EQUAL
%token EOF

%type <Syntax.t option> program
%start program
%% 

program
    : expr? EOF { $1 }

simple_expr
    : value = INT { Uint { value; span=Span.create $sloc } }
    | value = FLOAT { Ufloat { value; span=Span.create $sloc } }
    | TRUE { Ubool { value=true; span=Span.create $sloc } }
    | FALSE { Ubool { value=false; span=Span.create $sloc } }
    | value = IDENT { Uvar { value; span=Span.create $sloc } }
    | value = IDENT LBRACKET index = expr RBRACKET { Usubscript { value; index; span=Span.create $sloc } }  
    | name = IDENT EQUAL value = expr { Uassign { name; value; span=Span.create $sloc } }
    | value = IDENT LBRACKET index = expr RBRACKET EQUAL new_value = simple_expr { Uassign_subscript { value; index; new_value; span=Span.create $sloc } }
    | value = simple_expr DOT index = INT { Utuple_subscript { value; index; span=Span.create $sloc } }
    | value = simple_expr DOT field = IDENT { Uselect { value; field; span=Span.create $sloc } }
    | LBRACE fields = record_fields RBRACE { Urecord { fields; span=Span.create $sloc } }
    | LPAREN values = elements RPAREN { Utuple { values; span=Span.create $sloc } }
    | LBRACKET_BAR values = elements RBRACKET_BAR { Uvector { is_mutable=false; values; span=Span.create $sloc } }
    | MUT LBRACKET_BAR values = elements RBRACKET_BAR { Uvector { is_mutable=true; values; span=Span.create $sloc } }


expr
    : value = simple_expr { value }
    | fn = simple_expr value = expr  { Uapp{ fn; value; span=Span.create $sloc } }
    | MINUS value = expr {
        match value with
        | Uint { value; span } -> Uint { value=(-value); span }   
        | _ -> Uneg { value; span=Span.create $sloc }
    }
    | left = expr PLUS right = expr { Uop { op=Op.Add; left; right; span=Span.create $sloc } }
    | left = expr MINUS right = expr { Uop { op=Op.Sub; left; right; span=Span.create $sloc } }
    | left = expr STAR right = expr { Uop { op=Op.Mul; left; right; span=Span.create $sloc } }
    | left = expr SLASH right = expr { Uop { op=Op.Div; left; right; span=Span.create $sloc } }
    | left = expr PLUS_DOT right = expr { Uop { op=Op.FAdd; left; right; span=Span.create $sloc } }
    | left = expr MINUS_DOT right = expr { Uop { op=Op.FSub; left; right; span=Span.create $sloc } }
    | left = expr STAR_DOT right = expr { Uop { op=Op.FMul; left; right; span=Span.create $sloc } }
    | left = expr SLASH_DOT right = expr { Uop { op=Op.FDiv; left; right; span=Span.create $sloc } }
    | left = expr EQUAL_EQUAL right = expr { Uop { op=Op.Eq; left; right; span=Span.create $sloc } }
    | left = expr LESS right = expr { Uop { op=Op.Lt; left; right; span=Span.create $sloc } }
    | left = expr GREATER right = expr { Uop { op=Op.Gt; left; right; span=Span.create $sloc } }
    | left = expr LESS_EQUAL right = expr { Uop { op=Op.Leq; left; right; span=Span.create $sloc } }
    | left = expr GREATER_EQUAL right = expr { Uop { op=Op.Geq; left; right; span=Span.create $sloc } }
    | left = expr NOT_EQUAL right = expr { Uop { op=Op.Neq; left; right; span=Span.create $sloc } }
    | FOR iterates = iterates DO body = expr END { 
        let rec to_iterates = function
            | [] -> Udone
            | (name, start, is_ascending, finish, span) :: iterates -> 
                Uiterate { 
                    name; 
                    start; 
                    is_ascending; 
                    finish; 
                    rest = to_iterates iterates; 
                    span 
                } in
        let iterates = to_iterates iterates in
        Ufor { iterates; body; span=Span.create $sloc } 
    }
    | IF cond = expr THEN then_ = expr END { Uif_end { cond; then_; span=Span.create $sloc } }
    | IF cond = expr THEN then_ = expr ELSE else_ = expr { Uif { cond; then_; else_; span=Span.create $sloc } }
    | first = expr SEMICOLON second = expr { Useq { first; second; span=Span.create $sloc } }
    | LET binding = IDENT EQUAL value = expr IN app = expr { Ulet { binding; is_mutable=false; value; app; span=Span.create $sloc } }
    | LET MUT binding = IDENT EQUAL value = expr IN app = expr { Ulet { binding; is_mutable=true; value; app; span=Span.create $sloc } }
    | LET name = IDENT parameter = IDENT RIGHT_ARROW value = expr IN app = expr { 
        Ulet_fun { 
            name; 
            closure = Uclosure  { 
                    parameter; 
                    value; 
                    span = Span.create ($startpos(name), $startpos(value)) 
                }; 
                app;
                span = Span.create $sloc  
            }
        }
    | FN parameter = IDENT RIGHT_ARROW value = expr { 
        let lambda = Uclosure { parameter;  value; span=Span.create $sloc } in 
        Ulambda { closure=lambda }
    }
    | DEF name = IDENT parameter = IDENT EQUAL value = expr IN app = expr { 
        Udef { 
            name; 
            closure = Uclosure  { 
                    parameter; 
                    value; 
                    span = Span.create ($startpos(name), $startpos(value)) 
                }; 
                app;
                span = Span.create $sloc
            }      
        }

record_fields
    : fields = separated_nonempty_list(COMMA, record_field) { fields }

record_field
    : field = IDENT EQUAL value = simple_expr { (field, value) }

elements
    : separated_list(COMMA, expr) { $1 }

iterate
    : name = IDENT LEFT_ARROW start = expr TO finish = expr  { (name, start, true, finish, Span.create $sloc)  }
    | name = IDENT LEFT_ARROW start = expr DOWNTO finish = expr { (name, start, false, finish, Span.create $sloc)  }

iterates
    : iterates = separated_list(COMMA, iterate) { iterates }