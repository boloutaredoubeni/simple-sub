open Core
open Text

module Op = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Eq
    | Neq
    | Lt
    | Gt
    | Leq
    | Geq
    | FAdd
    | FSub
    | FMul
    | FDiv
  [@@deriving compare, sexp]
end

(*
   TODO: part 2. cubiml i.e. tags/pattern matching, 1st-class references, nullable, mutual recursion, strings, record extension/contraction, type annotation
   TODO: continuation based concurrency, interfaces, error/exceptions, tensors, suspend/resume continuations, typecasting, iterators, break, contnue, return, yield, mutable vs immutable strings, state vars, arrays of fixed length, slices, subsumption checking for polymorphic annotations
   TODO: more advanced type simplification, type monomorphization, pattern match compilation
   TODO: delimited ir
   TODO: ownership based reference counting
   TODO: compilation to WASM and/or LLVM
*)

module Mutability = struct
  module T = struct
    type t = Mutable | Immutable | Reference [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Readability = struct
  module T = struct
    type t = Readonly | Writeonly | ReadWrite [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

type t =
  | Uint of { value : int; span : (Span.span[@compare.ignore]) }
  | Ufloat of { value : float; span : (Span.span[@compare.ignore]) }
  | Ubool of { value : bool; span : (Span.span[@compare.ignore]) }
  | Uvar of { value : Symbol.t; span : (Span.span[@compare.ignore]) }
  | Uapp of { fn : t; value : t; span : (Span.span[@compare.ignore]) }
  | Uneg of { value : t; span : (Span.span[@compare.ignore]) }
  | Uop of {
      op : Op.t;
      left : t;
      right : t;
      span : (Span.span[@compare.ignore]);
    }
  | Utuple of { values : t list; span : (Span.span[@compare.ignore]) }
  | Uvector of {
      values : t list;
      mutability : Mutability.t;
      span : (Span.span[@compare.ignore]);
    }
  | Uslice of {
      value : Symbol.t;
      readability : Readability.t;
      span : (Span.span[@compare.ignore]);
    }
  | Uassign_subscript of {
      value : Symbol.t;
      index : t;
      new_value : t;
      span : (Span.span[@compare.ignore]);
    }
  | Uassign of {
      name : Symbol.t;
      value : t;
      span : (Span.span[@compare.ignore]);
    }
  | Uderef of { name : Symbol.t; span : (Span.span[@compare.ignore]) }
  | Uupdate_ref of {
      name : Symbol.t;
      value : t;
      span : (Span.span[@compare.ignore]);
    }
  | Utuple_subscript of {
      value : t;
      index : int;
      span : (Span.span[@compare.ignore]);
    }
  | Usubscript of {
      value : Symbol.t;
      index : t;
      span : (Span.span[@compare.ignore]);
    }
  | Urecord of {
      fields : (Symbol.t * t) list;
      span : (Span.span[@compare.ignore]);
    }
  | Uselect of {
      value : t;
      field : Symbol.t;
      span : (Span.span[@compare.ignore]);
    }
  | Useq of { first : t; second : t; span : (Span.span[@compare.ignore]) }
  | Ulet of {
      binding : Symbol.t;
      mutability : Mutability.t;
      value : t;
      app : t;
      span : (Span.span[@compare.ignore]);
    }
  | Ulet_fun of {
      name : Symbol.t;
      closure : closure;
      app : t;
      span : (Span.span[@compare.ignore]);
    }
  | Udef of {
      name : Symbol.t;
      closure : closure;
      app : t;
      span : (Span.span[@compare.ignore]);
    }
  | Ulambda of { closure : closure }
  | Uif_end of { cond : t; then_ : t; span : (Span.span[@compare.ignore]) }
  | Uif of {
      cond : t;
      then_ : t;
      else_ : t;
      span : (Span.span[@compare.ignore]);
    }
  | Ufor of {
      iterates : iterate;
      body : t;
      span : (Span.span[@compare.ignore]);
    }
[@@deriving compare, sexp]

and closure =
  | Uclosure of {
      parameter : Symbol.t;
      value : t;
      span : (Span.span[@compare.ignore]);
    }
[@@deriving compare, sexp]

and iterate =
  | Uiterate of {
      name : Symbol.t;
      start : t;
      finish : t;
      is_ascending : bool;
      span : (Span.span[@compare.ignore]);
      rest : iterate;
    }
  | Udone
[@@deriving compare, sexp]

let default = Uint { value = 0; span = Span.default }
let to_sexp_string t = Sexp.to_string (sexp_of_t t)

module Spanned : Span.SPANNED = struct
  type nonrec t = t [@@deriving compare, sexp]

  let span = function
    | Uint { span; _ }
    | Ubool { span; _ }
    | Ufloat { span; _ }
    | Uvar { span; _ }
    | Uapp { span; _ }
    | Urecord { span; _ }
    | Utuple { span; _ }
    | Uvector { span; _ }
    | Uslice { span; _ }
    | Utuple_subscript { span; _ }
    | Usubscript { span; _ }
    | Uselect { span; _ }
    | Ulet { span; _ }
    | Ulet_fun { span; _ }
    | Ulambda { closure = Uclosure { span; _ }; _ }
    | Udef { span; _ }
    | Uop { span; _ }
    | Uneg { span; _ }
    | Uif_end { span; _ }
    | Ufor { span; _ }
    | Uif { span; _ }
    | Useq { span; _ }
    | Uassign { span; _ }
    | Uderef { span; _ }
    | Uupdate_ref { span; _ }
    | Uassign_subscript { span; _ } ->
        span
end

module Closure = struct
  type nonrec t = closure [@@deriving compare, sexp]

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function Uclosure { span; _ } -> span
  end
end

module Tests = struct
  let%test_unit "different ast" =
    let left = Uvar { value = Symbol.of_string "x"; span = Span.default } in
    let right =
      Uvar
        {
          value = Symbol.of_string "x";
          span =
            Span
              {
                filename = "something else";
                start = Position.default;
                finish = Position.default;
              };
        }
    in
    [%test_result: int] (compare left right) ~expect:0
end
