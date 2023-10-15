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

module Scope = struct
  module T = struct
    type t = Scope of { value : int } [@@deriving compare, sexp]

    let default = Scope { value = 0 }
    let incr (Scope { value }) = Scope { value = value + 1 }
  end

  include T
  include Comparable.Make (T)
end

module rec Simple_type : sig
  type t =
    | Svar_type of { state : Variable.t }
    | Sint_type
    | Sfloat_type
    | Sbool_type
    | Smutable of { read : t; write : t; scope : Scope.t }
    | Sfunction_type of { argument : t; result : t }
    | Sunit_type
    | Ssparse_tuple of { indices : (int * t) list }
    | Stuple_type of { first : t; second : t; rest : t list }
      (* TODO: mutable types need 'read'/'write's
         should vectors and references be limited by scope and readonly/writeonly?

         i.e forall i. 0 <= i < n -> v[i]
         // this readonly
         let xs = [||] in
         // this is local readwrite, global readonly
         let xs = mut [||] in
         // this is global readwrite
         let xs = ref [||] in
         // this is pass readwrite
          f (&xs[..])
         // this is pass writeonly
         f (&mut xs[..])

         /// the  read/write reference version
         // this is local readwrite, global readonly
         let ref r = ... in
         // this is global readwrite
         let ref mut r = ... in
         // this is pass readonly
          f r
         // this is pass writeonly
         f (&mut r)
         // this is ref write
         r := 1
         // this is ref read
         *r OR ^r or @r // *r is the most common and preferred


         /// state vars, similar to useState hooks in react
         // this is user defined readwrite rules
         var set_x get_x = ... in
         // this is readonly
         ..x..
         // this is write only
         set_x 1
         // this is pass readwrite
         f (x, set_x)
         // this is pass readonly
         f x
         // this is pass writeonly
         f set_x
      *)
    | Svector_type of { read : t option; write : t option }
    | Srecord of { fields : (Symbol.t * t) list }
  [@@deriving compare, sexp]

  val level : t -> Level.t
  val deref : t -> t
end = struct
  type t =
    | Svar_type of { state : Variable.t }
    | Sint_type
    | Sfloat_type
    | Sbool_type
    | Smutable of { read : t; write : t; scope : Scope.t }
    | Sfunction_type of { argument : t; result : t }
    | Sunit_type
    | Ssparse_tuple of { indices : (int * t) list }
    | Stuple_type of { first : t; second : t; rest : t list }
    | Svector_type of { read : t option; write : t option }
    | Srecord of { fields : (Symbol.t * t) list }
  [@@deriving compare, sexp]

  let rec level = function
    | Sfunction_type { argument; result } ->
        Level.max (level argument) (level result)
    | Svar_type { state = VariableState { level; _ } } -> level
    | Sint_type | Sbool_type | Sfloat_type | Sunit_type -> Level.default
    | Ssparse_tuple { indices } ->
        List.fold indices ~init:Level.default ~f:(fun acc (_, t) ->
            Level.max acc (level t))
    | Stuple_type { first; second; rest } ->
        List.fold (first :: second :: rest) ~init:Level.default ~f:(fun acc t ->
            Level.max acc (level t))
    | Svector_type { read; write } ->
        List.fold [ read; write ] ~init:Level.default ~f:(fun acc -> function
          | None -> acc | Some t -> Level.max acc (level t))
    | Srecord { fields } ->
        List.fold fields ~init:Level.default ~f:(fun acc (_, t) ->
            Level.max acc (level t))
    | Smutable { read; write; _ } -> Level.max (level read) (level write)

  let rec deref = function Smutable { read; _ } -> deref read | type' -> type'
end

and Variable : sig
  type t =
    | VariableState of {
        name : Symbol.t;
        level : Level.t;
        lower_bounds : Simple_type.t list ref;
        upper_bounds : Simple_type.t list ref;
      }
  [@@deriving compare, sexp]

  val create : level:Level.t -> fresher:(module FRESH_SYM) -> Simple_type.t
  val name : t -> Symbol.t
  val level : t -> Level.t
  val lower_bounds : t -> Simple_type.t list ref
  val upper_bounds : t -> Simple_type.t list ref
  val to_simple_type : t -> Simple_type.t
  val of_simple_type : Simple_type.t -> t option

  type comparator_witness

  val comparator : (t, comparator_witness) Comparator.t
end = struct
  module T = struct
    open Simple_type

    type t =
      | VariableState of {
          name : Symbol.t;
          level : Level.t;
          lower_bounds : Simple_type.t list ref;
          upper_bounds : Simple_type.t list ref;
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
  [@@deriving sexp, compare]
end

module rec T : sig
  type t =
    | Tint of { value : int; span : (Span.span[@compare.ignore]) }
    | Tfloat of { value : float; span : (Span.span[@compare.ignore]) }
    | Tbool of { value : bool; span : (Span.span[@compare.ignore]) }
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
        value : t;
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
        value : t;
        index : t;
        new_value : t;
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
    | Tif_end of {
        cond : t;
        then_ : t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
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
        value : t;
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
        value : t;
        index : t;
        new_value : t;
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
    | Tif_end of {
        cond : t;
        then_ : t;
        span : (Span.span[@compare.ignore]);
        type' : Simple_type.t;
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
    | Tassign _ | Tassign_subscript _ | Tunit _ -> Sunit_type
    | Trecord { fields; _ } ->
        Srecord { fields = List.map fields ~f:(fun (k, v) -> (k, type_of v)) }
    | Tlambda { closure } -> Closure.type_of closure
    | Tlet { app; _ } -> type_of app
    | Tseq { second; _ } -> type_of second
    | Tselect { type'; _ } | Tapp { type'; _ } -> type'
    | Tdef { app; _ } -> type_of app
    | Tprimop { type'; _ }
    | Tif { type'; _ }
    | Tif_end { type'; _ }
    | Tfor { type'; _ } ->
        type'

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function
      | Tint { span; _ }
      | Tbool { span; _ }
      | Tfloat { span; _ }
      | Tvar { span; _ }
      | Tapp { span; _ }
      | Tunit { span; _ }
      | Ttuple { span; _ }
      | Tvector { span; _ }
      | Ttuple_subscript { span; _ }
      | Tsubscript { span; _ }
      | Tassign { span; _ }
      | Tassign_subscript { span; _ }
      | Trecord { span; _ }
      | Tselect { span; _ }
      | Tlet { span; _ }
      | Tseq { span; _ }
      | Tlambda { closure = Tclosure { span; _ } }
      | Tdef { span; _ }
      | Tprimop { span; _ }
      | Tif_end { span; _ }
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
