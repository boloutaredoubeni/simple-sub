open Core
open Text

module Type = struct
  module T = struct
    type t =
      | Ty_top
      | Ty_bottom
      | Ty_union of { lhs : t; rhs : t }
      | Ty_intersection of { lhs : t; rhs : t }
      | Ty_function of { argument : t; result : t }
      | Ty_unit
      | Ty_tuple of { first : t; second : t; rest : t list }
      | Ty_vector of { read : t; write : t }
      | Ty_readonly_vector of { element : t }
      | Ty_writeonly_vector of { element : t }
      | Ty_record of { fields : (Symbol.t * t) list }
      | Ty_recursive of { name : Symbol.t; body : t }
      | Ty_variable of { name : Symbol.t }
      | Ty_mutable of { read : t; write : t }
      | Ty_int
      | Ty_float
      | Ty_bool
    [@@deriving compare, sexp, equal]

    let rec to_string = function
      | Ty_top -> "any"
      | Ty_bottom -> "void"
      | Ty_mutable { read; write } ->
          "mut[+" ^ to_string read ^ ", -" ^ to_string write ^ "]"
      | Ty_union { lhs; rhs } -> to_string lhs ^ " | " ^ to_string rhs
      | Ty_intersection { lhs; rhs } -> to_string lhs ^ " & " ^ to_string rhs
      | Ty_function { argument; result } ->
          to_string argument ^ " -> " ^ to_string result
      | Ty_unit -> "()"
      | Ty_tuple { first; second; rest } ->
          let rest = List.map rest ~f:to_string in
          let rest = String.concat ~sep:", " rest in
          "(" ^ to_string first ^ ", " ^ to_string second ^ ", " ^ rest ^ ")"
      | Ty_vector { read; write } ->
          "vector[+" ^ to_string read ^ ", -" ^ to_string write ^ "]"
      | Ty_readonly_vector { element } ->
          "readonly vector[" ^ to_string element ^ "]"
      | Ty_writeonly_vector { element } ->
          "writeonly vector[" ^ to_string element ^ "]"
      | Ty_record { fields } ->
          let fields =
            List.map fields ~f:(fun (name, type') ->
                Symbol.to_string name ^ ": " ^ to_string type')
          in
          let fields = String.concat ~sep:", " fields in
          "{" ^ fields ^ "}"
      | Ty_recursive { name; body } ->
          "\\" ^ Symbol.to_string name ^ "." ^ to_string body
      | Ty_variable { name } -> "'" ^ Symbol.to_string name
      | Ty_float -> "float"
      | Ty_int -> "int"
      | Ty_bool -> "bool"
  end

  include T
  include Comparable.Make (T)
end

module rec T : sig
  type t =
    | Lint of { value : int; span : (Span.span[@compare.ignore]) }
    | Lfloat of { value : float; span : (Span.span[@compare.ignore]) }
    | Lbool of { value : bool; span : (Span.span[@compare.ignore]) }
    | Lvar of {
        value : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lprimop of {
        op : Primop.t;
        args : t list;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lapp of {
        fn : t;
        value : t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lunit of { span : (Span.span[@compare.ignore]) }
    | Ltuple of {
        first : t;
        second : t;
        rest : t list;
        span : (Span.span[@compare.ignore]);
      }
    | Lvector of {
        values : t list;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lsubscript of {
        value : Symbol.t;
        index : t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Ltuple_subscript of {
        value : t;
        index : int;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lassign of {
        name : Symbol.t * Type.t;
        value : t;
        span : (Span.span[@compare.ignore]);
      }
    | Lassign_subscript of {
        value : Symbol.t;
        index : t;
        new_value : t;
        span : (Span.span[@compare.ignore]);
      }
    | Lrecord of {
        fields : (Symbol.t * t) list;
        span : (Span.span[@compare.ignore]);
      }
    | Lselect of {
        value : t;
        field : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Llet of {
        binding : Symbol.t * Type.t;
        value : t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
    | Lseq of { first : t; second : t; span : (Span.span[@compare.ignore]) }
    | Ldef of {
        name : Symbol.t;
        fn_type : Type.t;
        closure : Closure.t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
    | Lif of {
        cond : t;
        then_ : t;
        else_ : t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lfor of {
        iterate : Iterate.t;
        body : t;
        type' : Type.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  val type_of : t -> Type.t
end = struct
  type t =
    | Lint of { value : int; span : (Span.span[@compare.ignore]) }
    | Lfloat of { value : float; span : (Span.span[@compare.ignore]) }
    | Lbool of { value : bool; span : (Span.span[@compare.ignore]) }
    | Lvar of {
        value : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lprimop of {
        op : Primop.t;
        args : t list;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lapp of {
        fn : t;
        value : t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lunit of { span : (Span.span[@compare.ignore]) }
    | Ltuple of {
        first : t;
        second : t;
        rest : t list;
        span : (Span.span[@compare.ignore]);
      }
    | Lvector of {
        values : t list;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lsubscript of {
        value : Symbol.t;
        index : t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Ltuple_subscript of {
        value : t;
        index : int;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lassign of {
        name : Symbol.t * Type.t;
        value : t;
        span : (Span.span[@compare.ignore]);
      }
    | Lassign_subscript of {
        value : Symbol.t;
        index : t;
        new_value : t;
        span : (Span.span[@compare.ignore]);
      }
    | Lrecord of {
        fields : (Symbol.t * t) list;
        span : (Span.span[@compare.ignore]);
      }
    | Lselect of {
        value : t;
        field : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Llet of {
        binding : Symbol.t * Type.t;
        value : t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
    | Lseq of { first : t; second : t; span : (Span.span[@compare.ignore]) }
    | Ldef of {
        name : Symbol.t;
        fn_type : Type.t;
        closure : Closure.t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
    | Lif of {
        cond : t;
        then_ : t;
        else_ : t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
    | Lfor of {
        iterate : Iterate.t;
        body : t;
        type' : Type.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  let rec type_of =
    let open Type in
    function
    | Lint _ -> Ty_int
    | Lfloat _ -> Ty_float
    | Lbool _ -> Ty_bool
    | Lunit _ | Lassign _ | Lassign_subscript _ -> Ty_unit
    | Lvar { type'; _ } -> type'
    | Ltuple { first; second; rest; _ } ->
        Ty_tuple
          {
            first = type_of first;
            second = type_of second;
            rest = List.map rest ~f:(fun t -> type_of t);
          }
    | Lvector { type'; _ } -> type'
    | Ltuple_subscript { type'; _ } -> type'
    | Lsubscript { type'; _ } -> type'
    | Lrecord { fields; _ } ->
        Ty_record { fields = List.map fields ~f:(fun (k, v) -> (k, type_of v)) }
    | Llet { app; _ } -> type_of app
    | Lseq { second; _ } -> type_of second
    | Lselect { type'; _ } -> type'
    | Lapp { type'; _ } -> type'
    | Ldef { app; _ } -> type_of app
    | Lprimop { type'; _ } -> type'
    | Lif { type'; _ } -> type'
    | Lfor { type'; _ } -> type'

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function
      | Lint { span; _ }
      | Lfloat { span; _ }
      | Lbool { span; _ }
      | Lunit { span; _ }
      | Lvar { span; _ }
      | Lapp { span; _ }
      | Ltuple { span; _ }
      | Lvector { span; _ }
      | Ltuple_subscript { span; _ }
      | Lsubscript { span; _ }
      | Lassign { span; _ }
      | Lassign_subscript { span; _ }
      | Lrecord { span; _ }
      | Lselect { span; _ }
      | Llet { span; _ }
      | Lseq { span; _ }
      | Ldef { span; _ }
      | Lprimop { span; _ }
      | Lif { span; _ }
      | Lfor { span; _ } ->
          span
  end
end

and Closure : sig
  type t =
    | Lclosure of {
        parameter : Symbol.t * Type.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
    | Lrec_closure of {
        self : Symbol.t;
        parameter : Symbol.t * Type.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  val type_of : t -> Type.t
end = struct
  type t =
    | Lclosure of {
        parameter : Symbol.t * Type.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
    | Lrec_closure of {
        self : Symbol.t;
        parameter : Symbol.t * Type.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  let type_of = function
    | Lclosure { parameter = _, type'; value; _ }
    | Lrec_closure { parameter = _, type'; value; _ } ->
        Type.Ty_function { argument = type'; result = T.type_of value }

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function
      | Lclosure { span; _ } | Lrec_closure { span; _ } -> span
  end
end

and Primop : sig
  type t =
    | Lint_add
    | Lint_sub
    | Lint_mul
    | Lint_div
    | Lint_neg
    | Lint_eq
    | Lint_neq
    | Lint_lt
    | Lint_le
    | Lint_gt
    | Lint_ge
    | Lfloat_add
    | Lfloat_sub
    | Lfloat_mul
    | Lfloat_div
    | Lfloat_neg
    | Lfloat_eq
    | Lfloat_neq
    | Lfloat_lt
    | Lfloat_le
    | Lfloat_gt
    | Lfloat_ge
    | Lbool_eq
    | Lbool_neq
  [@@deriving compare, sexp]
end = struct
  type t =
    | Lint_add
    | Lint_sub
    | Lint_mul
    | Lint_div
    | Lint_neg
    | Lint_eq
    | Lint_neq
    | Lint_lt
    | Lint_le
    | Lint_gt
    | Lint_ge
    | Lfloat_add
    | Lfloat_sub
    | Lfloat_mul
    | Lfloat_div
    | Lfloat_neg
    | Lfloat_eq
    | Lfloat_neq
    | Lfloat_lt
    | Lfloat_le
    | Lfloat_gt
    | Lfloat_ge
    | Lbool_eq
    | Lbool_neq
  [@@deriving compare, sexp]
end

and Iterate : sig
  type t =
    | Literate of {
        name : Symbol.t;
        start : T.t;
        finish : T.t;
        is_ascending : bool;
        rest : t;
        span : (Span.span[@compare.ignore]);
      }
    | Ldone
  [@@deriving compare, sexp]
end = struct
  type t =
    | Literate of {
        name : Symbol.t;
        start : T.t;
        finish : T.t;
        is_ascending : bool;
        rest : t;
        span : (Span.span[@compare.ignore]);
      }
    | Ldone
  [@@deriving compare, sexp]
end

include T
