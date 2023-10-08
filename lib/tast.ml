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
  end

  include T
  include Comparable.Make (T)
end

module rec SimpleType : sig
  type t =
    | Svar_type of { state : Variable.t }
    | Sint_type
    | Sfunction_type of { argument : t; result : t }
    | Srecord of { fields : (Symbol.t * t) list }
  [@@deriving compare, sexp]

  val level : t -> Level.t
end = struct
  type t =
    | Svar_type of { state : Variable.t }
    | Sint_type
    | Sfunction_type of { argument : t; result : t }
    | Srecord of { fields : (Symbol.t * t) list }
  [@@deriving compare, sexp]

  let rec level = function
    | Sfunction_type { argument; result } ->
        Level.max (level argument) (level result)
    | Svar_type { state = VariableState { level; _ } } -> level
    | Sint_type -> Level.default
    | Srecord { fields } ->
        List.fold fields ~init:Level.default ~f:(fun acc (_, t) ->
            Level.max acc (level t))
end

and Variable : sig
  type t =
    | VariableState of {
        name : Symbol.t;
        level : Level.t;
        lower_bounds : SimpleType.t list ref;
        upper_bounds : SimpleType.t list ref;
      }
  [@@deriving compare, sexp]

  val create : level:Level.t -> fresher:(module FRESH_SYM) -> SimpleType.t
  val name : t -> Symbol.t
  val level : t -> Level.t
  val lower_bounds : t -> SimpleType.t list ref
  val upper_bounds : t -> SimpleType.t list ref
  val to_simple_type : t -> SimpleType.t
  val of_simple_type : SimpleType.t -> t option

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end = struct
  module T = struct
    open SimpleType

    type t =
      | VariableState of {
          name : Symbol.t;
          level : Level.t;
          lower_bounds : SimpleType.t list ref;
          upper_bounds : SimpleType.t list ref;
        }
    [@@deriving compare, sexp]

    let name = function VariableState { name; _ } -> name
    let to_simple_type state = Svar_type { state }

    let of_simple_type = function
      | Svar_type { state; _ } -> Some state
      | _ -> None

    let create ~level ~fresher =
      let (module Fresh_sym : FRESH_SYM) = fresher in
      Svar_type
        {
          state =
            VariableState
              {
                level;
                lower_bounds = ref [];
                upper_bounds = ref [];
                name = Fresh_sym.f ();
              };
        }

    let level = function VariableState { level; _ } -> level

    let lower_bounds = function
      | VariableState { lower_bounds; _ } -> lower_bounds

    let upper_bounds = function
      | VariableState { upper_bounds; _ } -> upper_bounds
  end

  include T
  include Comparable.Make (T)
end

module PolymorhicType = struct
  type t = PolymorhicType of { level : Level.t; body : SimpleType.t }
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
      type t = PolarType of { type' : SimpleType.t; polar : polarity }
      [@@deriving sexp, compare]

      let create type' polar = PolarType { type'; polar }
    end

    include T
    include Comparable.Make (T)

    let level (PolarType { type'; _ }) = SimpleType.level type'

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

module rec T : sig
  type t =
    | Tint of { value : int; span : (Span.span[@compare.ignore]) }
    | Tvar of {
        value : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : SimpleType.t;
      }
    | Tapp of {
        fn : t;
        value : t;
        span : (Span.span[@compare.ignore]);
        type' : SimpleType.t;
      }
    | Trecord of {
        fields : (Symbol.t * t) list;
        span : (Span.span[@compare.ignore]);
      }
    | Tselect of {
        value : t;
        field : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : SimpleType.t;
      }
    | Tlet of {
        pattern : Pattern.t;
        value : t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tlambda of { closure : Closure.t }
    | Tdef of {
        name : Symbol.t;
        fn_type : SimpleType.t;
        closure : Closure.t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  val type_of : t -> SimpleType.t
end = struct
  type t =
    | Tint of { value : int; span : (Span.span[@compare.ignore]) }
    | Tvar of {
        value : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : SimpleType.t;
      }
    | Tapp of {
        fn : t;
        value : t;
        span : (Span.span[@compare.ignore]);
        type' : SimpleType.t;
      }
    | Trecord of {
        fields : (Symbol.t * t) list;
        span : (Span.span[@compare.ignore]);
      }
    | Tselect of {
        value : t;
        field : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : SimpleType.t;
      }
    | Tlet of {
        pattern : Pattern.t;
        value : t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
    | Tlambda of { closure : Closure.t }
    | Tdef of {
        name : Symbol.t;
        fn_type : SimpleType.t;
        closure : Closure.t;
        app : t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  let rec type_of = function
    | Tint _ -> SimpleType.Sint_type
    | Tvar { type'; _ } -> type'
    | Trecord { fields; _ } ->
        SimpleType.Srecord
          { fields = List.map fields ~f:(fun (k, v) -> (k, type_of v)) }
    | Tlambda { closure } -> Closure.type_of closure
    | Tlet { app; _ } -> type_of app
    | Tselect { type'; _ } -> type'
    | Tapp { type'; _ } -> type'
    | Tdef { app; _ } -> type_of app

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function
      | Tint { span; _ } -> span
      | Tvar { span; _ } -> span
      | Tapp { span; _ } -> span
      | Trecord { span; _ } -> span
      | Tselect { span; _ } -> span
      | Tlet { span; _ } -> span
      | Tlambda { closure = Tclosure { span; _ } } -> span
      | Tdef { span; _ } -> span
  end
end

and Closure : sig
  type t =
    | Tclosure of {
        parameter : Pattern.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  val type_of : t -> SimpleType.t
end = struct
  type t =
    | Tclosure of {
        parameter : Pattern.t;
        value : T.t;
        span : (Span.span[@compare.ignore]);
      }
  [@@deriving compare, sexp]

  let type_of = function
    | Tclosure { parameter = Tpat_var { type'; _ }; value; _ } ->
        SimpleType.Sfunction_type { argument = type'; result = T.type_of value }

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function Tclosure { span; _ } -> span
  end
end

and Pattern : sig
  type t =
    | Tpat_var of {
        value : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : SimpleType.t;
      }
  [@@deriving compare, sexp]
end = struct
  type t =
    | Tpat_var of {
        value : Symbol.t;
        span : (Span.span[@compare.ignore]);
        type' : SimpleType.t;
      }
  [@@deriving compare, sexp]

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function Tpat_var { span; _ } -> span
  end
end

module Tests = struct
  open T

  let%test_unit "different ast" =
    let left =
      Tvar
        {
          value = Symbol.of_string "x";
          span = Span.default;
          type' = SimpleType.Sint_type;
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
          type' = SimpleType.Sint_type;
        }
    in
    [%test_result: int] (compare left right) ~expect:0
end

include T
