open Core
open Text

module Op = struct
  type t = Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Leq | Geq
  [@@deriving compare, sexp]
end

(*
   TODO: tuples/unit, arrays, mut, for loop
*)

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
  | Usubscript of { value : t; index : t; span : (Span.span[@compare.ignore]) }
  | Urecord of {
      fields : (Symbol.t * t) list;
      span : (Span.span[@compare.ignore]);
    }
  | Uselect of {
      value : t;
      field : Symbol.t;
      span : (Span.span[@compare.ignore]);
    }
  | Ulet of {
      pattern : pat;
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
  | Uif of {
      cond : t;
      then_ : t;
      else_ : t;
      span : (Span.span[@compare.ignore]);
    }
[@@deriving compare, sexp]

and closure =
  | Uclosure of {
      parameter : pat;
      value : t;
      span : (Span.span[@compare.ignore]);
    }
[@@deriving compare, sexp]

and pat =
  | Upat_var of { value : Symbol.t; span : (Span.span[@compare.ignore]) }
[@@deriving compare, sexp]

let default = Uint { value = 0; span = Span.default }
let to_sexp_string t = Sexp.to_string (sexp_of_t t)

module Spanned : Span.SPANNED = struct
  type nonrec t = t [@@deriving compare, sexp]

  let span = function
    | Uint { span; _ } -> span
    | Ubool { span; _ } -> span
    | Ufloat { span; _ } -> span
    | Uvar { span; _ } -> span
    | Uapp { span; _ } -> span
    | Urecord { span; _ } -> span
    | Utuple { span; _ } -> span
    | Usubscript { span; _ } -> span
    | Uselect { span; _ } -> span
    | Ulet { span; _ } -> span
    | Ulet_fun { span; _ } -> span
    | Ulambda { closure = Uclosure { span; _ }; _ } -> span
    | Udef { span; _ } -> span
    | Uop { span; _ } -> span
    | Uneg { span; _ } -> span
    | Uif { span; _ } -> span
end

module Closure = struct
  type nonrec t = closure [@@deriving compare, sexp]

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function Uclosure { span; _ } -> span
  end
end

module Pattern = struct
  type nonrec t = pat [@@deriving compare, sexp]

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function Upat_var { span; _ } -> span
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
