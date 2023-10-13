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
      | Ty_record of { fields : (Symbol.t * t) list }
      | Ty_recursive of { name : Symbol.t; body : t }
      | Ty_variable of { name : Symbol.t }
      | Ty_int
      | Ty_float
      | Ty_bool
    [@@deriving compare, sexp, equal]

    let rec to_string = function
      | Ty_top -> "any"
      | Ty_bottom -> "void"
      | Ty_union { lhs; rhs } -> to_string lhs ^ " | " ^ to_string rhs
      | Ty_intersection { lhs; rhs } -> to_string lhs ^ " & " ^ to_string rhs
      | Ty_function { argument; result } ->
          to_string argument ^ " -> " ^ to_string result
      | Ty_unit -> "()"
      | Ty_tuple { first; second; rest } ->
          let rest = List.map rest ~f:to_string in
          let rest = String.concat ~sep:", " rest in
          "(" ^ to_string first ^ ", " ^ to_string second ^ ", " ^ rest ^ ")"
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
    | Lsubscript of {
        value : t;
        index : t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
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
        pattern : Bind.t;
        value : t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
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
    | Lsubscript of {
        value : t;
        index : t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
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
        pattern : Bind.t;
        value : t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
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
  [@@deriving compare, sexp]

  let rec type_of =
    let open Type in
    function
    | Lint _ -> Ty_int
    | Lfloat _ -> Ty_float
    | Lbool _ -> Ty_bool
    | Lunit _ -> Ty_unit
    | Lvar { type'; _ } -> type'
    | Ltuple { first; second; rest; _ } ->
        Ty_tuple
          {
            first = type_of first;
            second = type_of second;
            rest = List.map rest ~f:(fun t -> type_of t);
          }
    | Lsubscript { type'; _ } -> type'
    | Lrecord { fields; _ } ->
        Ty_record { fields = List.map fields ~f:(fun (k, v) -> (k, type_of v)) }
    | Llet { app; _ } -> type_of app
    | Lselect { type'; _ } -> type'
    | Lapp { type'; _ } -> type'
    | Ldef { app; _ } -> type_of app
    | Lprimop { type'; _ } -> type'
    | Lif { type'; _ } -> type'

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function
      | Lint { span; _ } -> span
      | Lfloat { span; _ } -> span
      | Lbool { span; _ } -> span
      | Lunit { span; _ } -> span
      | Lvar { span; _ } -> span
      | Lapp { span; _ } -> span
      | Ltuple { span; _ } -> span
      | Lsubscript { span; _ } -> span
      | Lrecord { span; _ } -> span
      | Lselect { span; _ } -> span
      | Llet { span; _ } -> span
      | Ldef { span; _ } -> span
      | Lprimop { span; _ } -> span
      | Lif { span; _ } -> span
  end
end

and Closure : sig
  type t =
    | Lclosure of {
        parameter : Bind.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
    | Lrec_closure of {
        self : Symbol.t;
        parameter : Bind.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  val type_of : t -> Type.t
end = struct
  type t =
    | Lclosure of {
        parameter : Bind.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
    | Lrec_closure of {
        self : Symbol.t;
        parameter : Bind.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  let type_of = function
    | Lclosure { parameter = Lparam { type'; _ }; value; _ }
    | Lrec_closure { parameter = Lparam { type'; _ }; value; _ } ->
        Type.Ty_function { argument = type'; result = T.type_of value }

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function
      | Lclosure { span; _ } | Lrec_closure { span; _ } -> span
  end
end

and Bind : sig
  type t =
    | Lparam of {
        value : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
  [@@deriving compare, sexp]
end = struct
  type t =
    | Lparam of {
        value : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Type.t;
      }
  [@@deriving compare, sexp]

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function Lparam { span; _ } -> span
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

include T
