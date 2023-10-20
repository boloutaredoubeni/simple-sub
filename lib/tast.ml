open Core
open Text

module Level = struct
  module T = struct
    type t = Level of { value : int } [@@deriving compare, sexp]

    let default = Level { value = 0 }
    let create level = Level { value = level }

    let max (Level { value = left }) (Level { value = right }) =
      Level { value = Int.max left right }

    let level_up (Level { value }) = Level { value = value + 1 }

    let to_string (Level { value }) =
      if value = 0 then "" else Printf.sprintf " @ %d" value
  end

  include T
  include Comparable.Make (T)
end

module Scope = struct
  module T = struct
    type t = Scope of { value : int } | Global [@@deriving compare, sexp]

    let global = Global
    let default = Scope { value = 0 }

    let incr = function
      | Scope { value } -> Scope { value = value + 1 }
      | Global -> Global

    let to_string = function
      | Scope { value } -> Printf.sprintf "@ %d" value
      | Global -> ""
  end

  include T
  include Comparable.Make (T)
end

module rec Simple_type : sig
  type t =
    | Svar_type of { state : Variable.t }
    | Sint_type
    | Schar_type
    | Sfloat_type
    | Sbool_type
    | Smutable of { read : t; write : t; scope : Scope.t }
    | Sreference of { read : t option; write : t option; scope : Scope.t }
    | Sfunction_type of { argument : t; result : t }
    | Sunit_type
    | Ssparse_tuple of { indices : (int * t) list }
    | Stuple_type of { first : t; second : t; rest : t list }
    | Sstring_type
    | Svector_type of { read : t option; write : t option; scope : Scope.t }
    | Srecord of { fields : (Symbol.t * t) list }
  [@@deriving compare, sexp]

  val level : t -> Level.t
  val deref : t -> t

  val to_string :
    visited:(Symbol.t, Symbol.comparator_witness) Set.t -> t -> string
end = struct
  type t =
    | Svar_type of { state : Variable.t }
    | Sint_type
    | Schar_type
    | Sfloat_type
    | Sbool_type
    | Smutable of { read : t; write : t; scope : Scope.t }
    | Sreference of { read : t option; write : t option; scope : Scope.t }
    | Sfunction_type of { argument : t; result : t }
    | Sunit_type
    | Ssparse_tuple of { indices : (int * t) list }
    | Stuple_type of { first : t; second : t; rest : t list }
    | Sstring_type
    | Svector_type of { read : t option; write : t option; scope : Scope.t }
    | Srecord of { fields : (Symbol.t * t) list }
  [@@deriving compare, sexp]

  let rec level = function
    | Sfunction_type { argument; result } ->
        Level.max (level argument) (level result)
    | Svar_type { state } -> state.level
    | Sint_type | Sbool_type | Sfloat_type | Sunit_type | Schar_type ->
        Level.default
    | Ssparse_tuple { indices } ->
        List.fold indices ~init:Level.default ~f:(fun acc (_, t) ->
            Level.max acc (level t))
    | Stuple_type { first; second; rest } ->
        List.fold (first :: second :: rest) ~init:Level.default ~f:(fun acc t ->
            Level.max acc (level t))
    | Sstring_type -> Level.default
    | Svector_type { read; write; _ } | Sreference { read; write; _ } ->
        List.fold [ read; write ] ~init:Level.default ~f:(fun acc -> function
          | None -> acc | Some t -> Level.max acc (level t))
    | Srecord { fields } ->
        List.fold fields ~init:Level.default ~f:(fun acc (_, t) ->
            Level.max acc (level t))
    | Smutable { read; write; _ } -> Level.max (level read) (level write)

  let deref = function Smutable { read = type'; _ } | type' -> type'

  let rec to_string ~visited = function
    | Svar_type { state } -> Variable.to_string ~visited state
    | Sint_type -> "int"
    | Sfloat_type -> "float"
    | Sbool_type -> "bool"
    | Schar_type -> "char"
    | Sunit_type -> "()"
    | Sstring_type -> "string"
    | Sfunction_type { argument; result } ->
        Printf.sprintf "%s -> %s"
          (to_string ~visited argument)
          (to_string ~visited result)
    | Smutable { read; write; scope } ->
        Printf.sprintf "mut[%s, %s] %s" (to_string ~visited read)
          (to_string ~visited write) (Scope.to_string scope)
    | Srecord { fields } ->
        Printf.sprintf "{%s}"
          (String.concat ~sep:", "
             (List.map fields ~f:(fun (k, v) ->
                  Printf.sprintf "%s: %s" (Symbol.to_string k)
                    (to_string ~visited v))))
    | Sreference { read; write; scope } ->
        Printf.sprintf "ref[%s, %s] %s"
          (Option.value_map read ~default:"_" ~f:(to_string ~visited))
          (Option.value_map write ~default:"_" ~f:(to_string ~visited))
          (Scope.to_string scope)
    | Stuple_type { first; second; rest } ->
        Printf.sprintf "(%s, %s%s)" (to_string ~visited first)
          (to_string ~visited second)
          (String.concat ~sep:", " (List.map rest ~f:(to_string ~visited)))
    | Ssparse_tuple { indices } ->
        Printf.sprintf "(%s)"
          (String.concat ~sep:", "
             (List.map indices ~f:(fun (i, t) ->
                  Printf.sprintf "%d: %s" i (to_string ~visited t))))
    | Svector_type { read; write; scope } ->
        Printf.sprintf "vector[%s, %s] %s"
          (Option.value_map read ~default:"_" ~f:(to_string ~visited))
          (Option.value_map write ~default:"_" ~f:(to_string ~visited))
          (Scope.to_string scope)
end

and Variable : sig
  type t = {
    name : Symbol.t;
    level : Level.t;
    mutable lower_bounds : Simple_type.t list;
    mutable upper_bounds : Simple_type.t list;
  }
  [@@deriving compare, sexp]

  val create : level:Level.t -> fresher:(module FRESH_SYM) -> Simple_type.t
  val name : t -> Symbol.t
  val level : t -> Level.t
  val to_simple_type : t -> Simple_type.t
  val of_simple_type : Simple_type.t -> t option

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t

  val to_string :
    visited:(Symbol.t, Symbol.comparator_witness) Set.t -> t -> string
end = struct
  module T = struct
    open Simple_type

    type t = {
      name : Symbol.t;
      level : Level.t;
      mutable lower_bounds : Simple_type.t list;
      mutable upper_bounds : Simple_type.t list;
    }
    [@@deriving compare, sexp]

    let name = function { name; _ } -> name
    let to_simple_type state = Svar_type { state }

    let of_simple_type = function
      | Svar_type { state; _ } -> Some state
      | _ -> None

    let create ~level ~fresher =
      let (module Fresh_sym : FRESH_SYM) = fresher in
      Svar_type
        {
          state =
            {
              level;
              lower_bounds = [];
              upper_bounds = [];
              name = Fresh_sym.f ();
            };
        }

    let level = function { level; _ } -> level

    let to_string ~visited state =
      if Set.mem visited state.name then Symbol.to_string state.name
      else
        let visited = Set.add visited state.name in
        (* FIXME: these have cycles, keep track of all printed vars and prefix them as params with constrain *)
        let lower_bounds =
          List.map state.lower_bounds ~f:(fun bound ->
              Simple_type.to_string ~visited bound)
        in
        let lower_bounds = String.concat ~sep:" | " lower_bounds in
        let upper_bounds =
          List.map state.upper_bounds ~f:(fun bound ->
              Simple_type.to_string ~visited bound)
        in
        let upper_bounds = String.concat ~sep:" | " upper_bounds in
        Printf.sprintf "%s%s'%s%s%s%s" lower_bounds
          (if String.is_empty lower_bounds then "" else " <: ")
          (Symbol.to_string state.name)
          (if String.is_empty upper_bounds then "" else " <: ")
          upper_bounds
          (Level.to_string state.level)
  end

  include T
  include Comparable.Make (T)
end

module PolymorhicType = struct
  type t = PolymorhicType of { level : Level.t; body : Simple_type.t }
  [@@deriving compare, sexp]

  let create level body = PolymorhicType { level; body }
end

module Polar = struct
  type polarity = Positive | Negative [@@deriving sexp, compare]

  let not = function Positive -> Negative | Negative -> Positive
  let bool = function Positive -> true | Negative -> false
  let of_bool = function true -> Positive | false -> Negative

  module Type = struct
    module T = struct
      type t = PolarType of { type' : Simple_type.t; polar : polarity }
      [@@deriving sexp, compare]

      let create type' polar = PolarType { type'; polar }
    end

    include T
    include Comparable.Make (T)

    let level (PolarType { type'; _ }) = Simple_type.level type'

    let not (PolarType { type'; polar }) =
      PolarType { type'; polar = not polar }

    let bool (PolarType { polar; _ }) = bool polar
    let polarity (PolarType { polar; _ }) = polar
    let type_of (PolarType { type'; _ }) = type'
  end

  module Variable = struct
    module T = struct
      type t = PolarVariable of { state : Variable.t; polar : polarity }
      [@@deriving sexp, compare]

      let create state polar = PolarVariable { state; polar }
    end

    include T
    include Comparable.Make (T)

    let level (PolarVariable { state; _ }) = Variable.level state

    let not (PolarVariable { state; polar }) =
      PolarVariable { state; polar = not polar }

    let bool (PolarVariable { polar; _ }) = bool polar
    let polarity (PolarVariable { polar; _ }) = polar
  end
end

module Type = struct
  module T = struct
    type t =
      | Ty_any
      | Ty_void
      | Ty_union of { lhs : t; rhs : t }
      | Ty_intersection of { lhs : t; rhs : t }
      | Ty_function of { argument : t; result : t }
      | Ty_unit
      | Ty_tuple of { first : t; second : t; rest : t list }
      | Ty_vector of { read : t; write : t }
      | Ty_reference of { read : t; write : t }
      | Ty_record of { fields : (Symbol.t * t) list }
      | Ty_recursive of { name : Symbol.t; body : t }
      | Ty_variable of { name : Symbol.t }
      | Ty_mutable of { read : t; write : t }
      | Ty_int
      | Ty_float
      | Ty_bool
      | Ty_char
      | Ty_string
    [@@deriving compare, sexp, equal]

    let rec to_string = function
      | Ty_any -> "any"
      | Ty_void -> "void"
      | Ty_mutable { read; write } ->
          "mut[" ^ to_string read ^ ", " ^ to_string write ^ "]"
      | Ty_union { lhs; rhs } -> to_string lhs ^ " | " ^ to_string rhs
      | Ty_intersection { lhs; rhs } -> to_string lhs ^ " + " ^ to_string rhs
      | Ty_function { argument; result } ->
          to_string argument ^ " -> " ^ to_string result
      | Ty_unit -> "()"
      | Ty_tuple { first; second; rest } ->
          let rest = List.map rest ~f:to_string in
          let rest = String.concat ~sep:", " rest in
          let rest = if String.is_empty rest then rest else ", " ^ rest in
          "(" ^ to_string first ^ ", " ^ to_string second ^ rest ^ ")"
      | Ty_vector { read; write } ->
          "vector[" ^ to_string read ^ ", " ^ to_string write ^ "]"
      | Ty_reference { read; write } ->
          let read = to_string read in
          let write = to_string write in
          "ref[" ^ read ^ ", " ^ write ^ "]"
      | Ty_record { fields } ->
          let fields =
            List.map fields ~f:(fun (name, type') ->
                Symbol.to_string name ^ ": " ^ to_string type')
          in
          let fields = String.concat ~sep:", " fields in
          "{" ^ fields ^ "}"
      | Ty_recursive { name; body } ->
          to_string body ^ " as " ^ Symbol.to_string name
      | Ty_variable { name } -> "'" ^ Symbol.to_string name
      | Ty_float -> "float"
      | Ty_int -> "int"
      | Ty_bool -> "bool"
      | Ty_char -> "char"
      | Ty_string -> "string"
  end

  include T
  include Comparable.Make (T)
end

module Primop = struct
  type t =
    | Tint_add
    | Tint_sub
    | Tint_mul
    | Tint_div
    | Tint_eq
    | Tint_lt
    | Tint_gt
    | Tint_le
    | Tint_ge
    | Tint_ne
    | Tint_neg
    | Tfloat_add
    | Tfloat_sub
    | Tfloat_mul
    | Tfloat_div
    | Tfloat_eq
    | Tfloat_lt
    | Tfloat_gt
    | Tfloat_le
    | Tfloat_ge
    | Tfloat_ne
    | Tfloat_neg
    | Tbool_eq
    | Tbool_ne
    | Tstr_concat
    | Tvector_concat
  [@@deriving sexp, compare]
end

module rec T : sig
  type t =
    | Tint of { value : int; span : (Span.span[@compare.ignore]) }
    | Tfloat of { value : float; span : (Span.span[@compare.ignore]) }
    | Tbool of { value : bool; span : (Span.span[@compare.ignore]) }
    | Tchar of { value : char; span : (Span.span[@compare.ignore]) }
    | Tstring of { value : string; span : (Span.span[@compare.ignore]) }
    | Tvar of {
        value : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tprimop of {
        op : Primop.t;
        args : t list;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tapp of {
        fn : t;
        value : t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tunit of { span : (Span.span[@compare.ignore]) }
    | Ttuple of {
        first : t;
        second : t;
        rest : t list;
        span : (Span.span[@compare.ignore]);
      }
    | Tvector of {
        values : t list;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Ttuple_subscript of {
        value : t;
        index : int;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tsubscript of {
        value : Symbol.t;
        index : t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tassign of {
        name : Symbol.t * Simple_type.t;
        value : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tassign_subscript of {
        name : Symbol.t;
        index : t;
        value : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tderef of {
        name : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tupdate_ref of {
        name : Symbol.t;
        value : t;
        span : (Span.span[@compare.ignore]);
      }
    | Trecord of {
        fields : (Symbol.t * t) list;
        span : (Span.span[@compare.ignore]);
      }
    | Tselect of {
        value : t;
        field : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tlet of {
        binding : Symbol.t * Simple_type.t;
        value : t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tseq of { first : t; second : t; span : (Span.span[@compare.ignore]) }
    | Tlambda of { closure : Closure.t }
    | Tdef of {
        name : Symbol.t;
        fn_type : Simple_type.t;
        closure : Closure.t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tif of {
        cond : t;
        then_ : t;
        else_ : t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tfor of {
        iterate : Iterate.t;
        body : t;
        type' : Simple_type.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  val type_of : t -> Simple_type.t
end = struct
  type t =
    | Tint of { value : int; span : (Span.span[@compare.ignore]) }
    | Tfloat of { value : float; span : (Span.span[@compare.ignore]) }
    | Tbool of { value : bool; span : (Span.span[@compare.ignore]) }
    | Tchar of { value : char; span : (Span.span[@compare.ignore]) }
    | Tstring of { value : string; span : (Span.span[@compare.ignore]) }
    | Tvar of {
        value : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tprimop of {
        op : Primop.t;
        args : t list;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tapp of {
        fn : t;
        value : t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tunit of { span : (Span.span[@compare.ignore]) }
    | Ttuple of {
        first : t;
        second : t;
        rest : t list;
        span : (Span.span[@compare.ignore]);
      }
    | Tvector of {
        values : t list;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Ttuple_subscript of {
        value : t;
        index : int;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tsubscript of {
        value : Symbol.t;
        index : t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tassign of {
        name : Symbol.t * Simple_type.t;
        value : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tassign_subscript of {
        name : Symbol.t;
        index : t;
        value : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tderef of {
        name : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tupdate_ref of {
        name : Symbol.t;
        value : t;
        span : (Span.span[@compare.ignore]);
      }
    | Trecord of {
        fields : (Symbol.t * t) list;
        span : (Span.span[@compare.ignore]);
      }
    | Tselect of {
        value : t;
        field : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tlet of {
        binding : Symbol.t * Simple_type.t;
        value : t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tseq of { first : t; second : t; span : (Span.span[@compare.ignore]) }
    | Tlambda of { closure : Closure.t }
    | Tdef of {
        name : Symbol.t;
        fn_type : Simple_type.t;
        closure : Closure.t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tif of {
        cond : t;
        then_ : t;
        else_ : t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
      }
    | Tfor of {
        iterate : Iterate.t;
        body : t;
        type' : Simple_type.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  let rec type_of =
    let open Simple_type in
    function
    | Tint _ -> Sint_type
    | Tfloat _ -> Sfloat_type
    | Tbool _ -> Sbool_type
    | Tchar _ -> Schar_type
    | Tstring _ -> Sstring_type
    | Tvar { type'; _ } -> type'
    | Ttuple { first; second; rest; _ } ->
        Stuple_type
          {
            first = type_of first;
            second = type_of second;
            rest = List.map rest ~f:type_of;
          }
    | Tvector { type'; _ } -> type'
    | Ttuple_subscript { type'; _ } | Tsubscript { type'; _ } -> type'
    | Tassign _ | Tassign_subscript _ | Tunit _ | Tupdate_ref _ -> Sunit_type
    | Trecord { fields; _ } ->
        Srecord { fields = List.map fields ~f:(fun (k, v) -> (k, type_of v)) }
    | Tlambda { closure } -> Closure.type_of closure
    | Tlet { app; _ } -> type_of app
    | Tseq { second; _ } -> type_of second
    | Tselect { type'; _ } | Tapp { type'; _ } -> type'
    | Tdef { app; _ } -> type_of app
    | Tprimop { type'; _ }
    | Tif { type'; _ }
    | Tderef { type'; _ }
    | Tfor { type'; _ } ->
        type'

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function
      | Tint { span; _ }
      | Tbool { span; _ }
      | Tfloat { span; _ }
      | Tchar { span; _ }
      | Tstring { span; _ }
      | Tvar { span; _ }
      | Tapp { span; _ }
      | Tunit { span; _ }
      | Ttuple { span; _ }
      | Tvector { span; _ }
      | Ttuple_subscript { span; _ }
      | Tsubscript { span; _ }
      | Tassign { span; _ }
      | Tassign_subscript { span; _ }
      | Tderef { span; _ }
      | Tupdate_ref { span; _ }
      | Trecord { span; _ }
      | Tselect { span; _ }
      | Tlet { span; _ }
      | Tseq { span; _ }
      | Tlambda { closure = Tclosure { span; _ } }
      | Tdef { span; _ }
      | Tprimop { span; _ }
      | Tfor { span; _ }
      | Tif { span; _ } ->
          span
  end
end

and Closure : sig
  type t =
    | Tclosure of {
        parameter : Symbol.t * Simple_type.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  val type_of : t -> Simple_type.t
end = struct
  type t =
    | Tclosure of {
        parameter : Symbol.t * Simple_type.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  let type_of = function
    | Tclosure { parameter = _, type'; value; _ } ->
        Simple_type.Sfunction_type
          { argument = type'; result = T.type_of value }

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function Tclosure { span; _ } -> span
  end
end

and Iterate : sig
  type t =
    | Titerate of {
        name : Symbol.t * Simple_type.t;
        start : T.t;
        finish : T.t;
        is_ascending : bool;
        rest : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tdone
  [@@deriving compare, sexp]
end = struct
  type t =
    | Titerate of {
        name : Symbol.t * Simple_type.t;
        start : T.t;
        finish : T.t;
        is_ascending : bool;
        rest : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tdone
  [@@deriving compare, sexp]
end

module Tests = struct
  open T

  let%test_unit "different ast" =
    let left =
      Tvar
        {
          value = Symbol.of_string "x";
          span = Span.default;
          type' = Simple_type.Sint_type;
        }
    in
    let right =
      Tvar
        {
          value = Symbol.of_string "x";
          span =
            Span
              {
                filename = "something else";
                start = Position.default;
                finish = Position.default;
              };
          type' = Simple_type.Sint_type;
        }
    in
    [%test_result: int] (compare left right) ~expect:0
end

include T
