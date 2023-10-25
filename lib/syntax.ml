open Core
open Text

module Op = struct
  type t =
    | SAdd
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

  let to_string = function
    | SAdd -> "^"
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Leq -> "<="
    | Geq -> ">="
    | FAdd -> "+."
    | FSub -> "-."
    | FMul -> "*."
    | FDiv -> "/."
end

(*
   TODO: (mutable records, record slices), type annotation, (let tuple, record pattern and function case, record and tuple patterns), implicit, suspendable futures
   TODO: Phases: Untyped -> Typed -> Lambda -> Mono -> ANF -> CPS -> SSA -> LLVM
*)

module Mutability = struct
  module T = struct
    type t = Mutable | Immutable | Reference | MutableReference
    [@@deriving compare, sexp]

    let to_string = function
      | Mutable -> "mut"
      | Immutable -> ""
      | Reference -> "ref"
      | MutableReference -> "ref mut"
  end

  include T
  include Comparable.Make (T)
end

module Readability = struct
  module T = struct
    type t = Readonly | Writeonly | ReadWrite
    [@@deriving compare, sexp, equal]

    let to_string = function
      | Readonly -> ""
      | Writeonly -> failwith "writeonly not implemented"
      | ReadWrite -> "&mut"
  end

  include T
  include Comparable.Make (T)
end

type t =
  | Uint of { value : int; span : (Span.span[@compare.ignore]) }
  | Ufloat of { value : float; span : (Span.span[@compare.ignore]) }
  | Ubool of { value : bool; span : (Span.span[@compare.ignore]) }
  | Uchar of { value : char; span : (Span.span[@compare.ignore]) }
  | Ustring of { value : string; span : (Span.span[@compare.ignore]) }
  | Uvar of { value : Symbol.t; span : (Span.span[@compare.ignore]) }
  | Ucase of { case : Symbol.t; value : t; span : (Span.span[@compare.ignore]) }
  | Umatch of { value : t; cases : alt; span : (Span.span[@compare.ignore]) }
  | Uapp of { fn : t; value : t; span : (Span.span[@compare.ignore]) }
  | Uneg of { value : t; span : (Span.span[@compare.ignore]) }
  | Uop of {
      op : Op.t;
      left : t;
      right : t;
      span : (Span.span[@compare.ignore]);
    }
  | Utuple of { values : t list; span : (Span.span[@compare.ignore]) }
  | Ulist of {
      values : t list;
      mutability : Mutability.t;
      span : (Span.span[@compare.ignore]);
    }
  | Uslice of {
      value : Symbol.t;
      start : t option;
      finish : t option;
      readability : Readability.t;
      span : (Span.span[@compare.ignore]);
    }
  | Uassign_subscript of {
      name : Symbol.t;
      index : t;
      value : t;
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
  | Umutable_ref of { value : Symbol.t; span : (Span.span[@compare.ignore]) }
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
      proto : Symbol.t option;
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
  | Uresume of {
      continuation : Symbol.t;
      value : t;
      span : (Span.span[@compare.ignore]);
    }
[@@deriving compare, sexp]

and closure =
  | Uclosure of {
      parameter : Symbol.t;
      value : t;
      span : (Span.span[@compare.ignore]);
    }
  | Ususpend of {
      continuation : Symbol.t;
      body : t;
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

and alt =
  | Ualt of {
      tag : Symbol.t;
      name : Symbol.t option;
      expr : t;
      span : (Span.span[@compare.ignore]);
      rest : alt;
    }
  | Uno_match

let default = Utuple { values = []; span = Span.default }
let to_sexp_string t = Sexp.to_string (sexp_of_t t)

module Spanned : Span.SPANNED = struct
  type nonrec t = t [@@deriving compare, sexp]

  let span = function
    | Uint { span; _ }
    | Ubool { span; _ }
    | Uchar { span; _ }
    | Ufloat { span; _ }
    | Uvar { span; _ }
    | Uapp { span; _ }
    | Ucase { span; _ }
    | Umatch { span; _ }
    | Urecord { span; _ }
    | Utuple { span; _ }
    | Ulist { span; _ }
    | Ustring { span; _ }
    | Uslice { span; _ }
    | Utuple_subscript { span; _ }
    | Usubscript { span; _ }
    | Uselect { span; _ }
    | Ulet { span; _ }
    | Ulet_fun { span; _ }
    | Ulambda { closure = Uclosure { span; _ }; _ }
    | Ulambda { closure = Ususpend { span; _ }; _ }
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
    | Umutable_ref { span; _ }
    | Uresume { span; _ }
    | Uassign_subscript { span; _ } ->
        span
end

module Closure = struct
  type nonrec t = closure [@@deriving compare, sexp]

  module Spanned : Span.SPANNED = struct
    type nonrec t = t [@@deriving compare, sexp]

    let span = function Uclosure { span; _ } | Ususpend { span; _ } -> span
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
