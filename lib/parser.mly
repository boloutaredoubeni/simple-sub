%{
    open Syntax
    open Text
%}

%token <Int.t> INT
%token <Float.t> FLOAT
%token <Text.Symbol.t> IDENT
%token <Text.Symbol.t> CASE_IDENT
%token <Char.t> CHAR
%token <String.t> STRING
%token LET 
%token MUT REF
%token COLON_EQUAL BANG
%token EQUAL 
%token LEFT_ARROW RIGHT_ARROW WIDE_ARROW
%token IN
%token WITH
%token DEF FN
%token SUSPEND
%token RESUME
%token MATCH CASE
%token AS_MUT
%token DOT DOT_DOT
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
%token CARET 
%token PLUS MINUS STAR SLASH 
%token PLUS_DOT MINUS_DOT STAR_DOT SLASH_DOT
%token EQUAL_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL NOT_EQUAL
%token EOF


%type <Syntax.t option> program
%start program
%% 

program
    : expr = expr? EOF { expr }

simple_expr
    : value = INT { Uint { value; span=Span.create $sloc } }
    | value = FLOAT { Ufloat { value; span=Span.create $sloc } }
    | TRUE { Ubool { value=true; span=Span.create $sloc } }
    | FALSE { Ubool { value=false; span=Span.create $sloc } }
    | CHAR { Uchar { value=$1; span=Span.create $sloc } }
    | STRING { Ustring { value=$1; span=Span.create $sloc } }
    | value = IDENT { Uvar { value; span=Span.create $sloc } }
    | value = IDENT LBRACKET index = expr RBRACKET { Usubscript { value; index; span=Span.create $sloc } }  
    | name = IDENT EQUAL value = expr { Uassign { name; value; span=Span.create $sloc } }
    | name = IDENT LBRACKET index = expr RBRACKET EQUAL value = simple_expr { Uassign_subscript { name; index; value; span=Span.create $sloc } }
    | value = simple_expr DOT index = INT { Utuple_subscript { value; index; span=Span.create $sloc } }
    | value = simple_expr DOT field = IDENT { Uselect { value; field; span=Span.create $sloc } }
    | LBRACE fields = record_fields RBRACE { Urecord { proto=None; fields; span=Span.create $sloc } }
    | LBRACE proto = IDENT WITH fields = record_fields RBRACE { Urecord { proto=Some proto; fields; span=Span.create $sloc } }
    | LPAREN values = elements RPAREN { Utuple { values; span=Span.create $sloc } }
    | mutability = mutability LBRACKET_BAR values = elements RBRACKET_BAR { Ulist { mutability; values; span=Span.create $sloc } }
    | AS_MUT value = IDENT LBRACKET start = simple_expr? DOT_DOT finish = simple_expr? RBRACKET { Uslice { readability=Readability.ReadWrite; value; span=Span.create $sloc; start; finish } }
    | AS_MUT value = IDENT { Umutable_ref { value; span=Span.create $sloc }}
    | value = IDENT LBRACKET start = simple_expr? DOT_DOT finish = simple_expr? RBRACKET { Uslice { readability=Readability.Readonly; value; span=Span.create $sloc; start; finish } }
    | name = IDENT COLON_EQUAL value = simple_expr { Uupdate_ref { name; value; span=Span.create $sloc } }
    | BANG name = IDENT { Uderef { name; span=Span.create $sloc } }
    | MINUS value = expr {
        match value with
        | Uint { value; span } -> Uint { value=(-value); span }   
        | _ -> Uneg { value; span=Span.create $sloc }
    }
    | left = expr CARET right = expr { Uop { op=Op.SAdd; left; right; span=Span.create $sloc } }
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
    | case = CASE_IDENT value = simple_expr { Ucase { case; value; span=Span.create $sloc } }
    | case = CASE_IDENT { Ucase { case; value=Utuple { values=[]; span=Span.create $sloc }; span=Span.create $sloc } }
    | fn = simple_expr value = expr  { Uapp{ fn; value; span=Span.create $sloc } }

mutability
    : MUT { Mutability.Mutable }
    | REF { Mutability.Reference }
    | { Mutability.Immutable }

expr
    : value = simple_expr { value }
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
    | LET binding = IDENT EQUAL value = expr IN app = expr { Ulet { binding; mutability=Mutability.Immutable; value; app; span=Span.create $sloc } }
    | LET MUT binding = IDENT EQUAL value = expr IN app = expr { Ulet { binding; mutability=Mutability.Mutable; value; app; span=Span.create $sloc } }
    | LET REF binding = IDENT EQUAL value = expr IN app = expr { Ulet { binding; mutability=Mutability.Reference; value; app; span=Span.create $sloc } }
    | LET REF MUT binding = IDENT EQUAL value = expr IN app = expr { Ulet { binding; mutability=Mutability.MutableReference; value; app; span=Span.create $sloc } }
    | LET name = IDENT parameter = IDENT RIGHT_ARROW value = expr IN app = expr { 
        Ulet_fun { 
            name; 
            closure = Uclosure  { 
                    parameter; 
                    value; 
                    span = Span.create ($startpos(parameter), $startpos(value))   
                }; 
                app;
                span = Span.create $sloc  
            }
        }

    | LET name = IDENT parameter = IDENT continuation = IDENT WIDE_ARROW value = expr IN app = expr { 
        Ulet_fun { 
            name; 
            closure = Uclosure  { 
                    parameter; 
                    value = Ulambda {
                        
                        closure = Ususpend {
                            continuation;
                            body = value;
                            span = Span.create ($startpos(name), $startpos(value))
                        };
                    };
                    span = Span.create ($startpos(parameter), $startpos(value))
                    }; 
                app;
                span = Span.create $sloc  
            }
    }

    | FN parameter = IDENT RIGHT_ARROW value = expr END { 
        let lambda = Uclosure { parameter;  value; span=Span.create $sloc } in 
        Ulambda { closure=lambda }
    }

    | FN parameter = IDENT continuation = IDENT WIDE_ARROW body = expr END { 
        let lambda = Ususpend { continuation;  body; span=Span.create $sloc } in 
        Ulambda { closure=Uclosure { parameter; value=Ulambda { closure=lambda }; span=Span.create $sloc
         } }
    }

    | DEF name = IDENT parameter = IDENT EQUAL value = expr IN app = expr { 
        Udef { 
            name; 
            closure = Uclosure  { 
                    parameter; 
                    value; 
                    span = Span.create ($startpos(parameter), $startpos(value))  
                }; 
                app;
                span = Span.create $sloc
            }      
        }

    | DEF name = IDENT parameter = IDENT continuation= IDENT WIDE_ARROW value = expr IN app = expr { 
        Udef { 
            name; 
            closure = Uclosure  { 
                    parameter; 
                    value = 
                    Ulambda {
                        closure =
                    Ususpend {
                        continuation;
                        body = value;
                        span = Span.create ($startpos(name), $startpos(value))
                    };
                    };
                    span = Span.create ($startpos(parameter), $startpos(value))
                };
                app;
                span = Span.create $sloc
            }      
        }

    | MATCH value = simple_expr cases = cases END { 
        let rec to_cases = function
            | [] -> Uno_match
            | (tag, name, expr, span) :: cases -> 
                Ualt { tag; name; expr; rest = to_cases cases; span }
             in
        let cases = to_cases cases in
        Umatch { value; cases; span=Span.create $sloc } }

    | RESUME continuation= IDENT value = simple_expr { Uresume { continuation; value; span=Span.create $sloc } }
    | SUSPEND continuation = IDENT WIDE_ARROW body = expr END {     
        let lambda = Ususpend { continuation; body; span=Span.create $sloc } in
        Ulambda { closure=lambda }
    }



record_fields
    : fields = separated_nonempty_list(COMMA, record_field) { fields }

record_field
    : field = IDENT EQUAL value = simple_expr { (field, value) }
    | field = IDENT { (field, Uvar { value=field; span=Span.create $sloc }) }


cases
    : cases = separated_nonempty_list(COMMA, case) { cases }

case 
    : CASE case = CASE_IDENT value = IDENT RIGHT_ARROW expr = expr { (case, Some value, expr, Span.create $sloc) }
    | CASE case = CASE_IDENT RIGHT_ARROW expr = expr { (case, None, expr, Span.create $sloc) }


elements
    : separated_list(COMMA, expr) { $1 }

iterate
    : name = IDENT LEFT_ARROW start = expr TO finish = expr  { (name, start, true, finish, Span.create $sloc)  }
    | name = IDENT LEFT_ARROW start = expr DOWNTO finish = expr { (name, start, false, finish, Span.create $sloc)  }

iterates
    : iterates = separated_list(COMMA, iterate) { iterates }