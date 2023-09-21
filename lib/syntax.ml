open Base

module Symbol = struct
  type t = Symbol of string [@@deriving compare, sexp]

  let of_string s = Symbol s
end

type t =
  | Uint of int
  | Uvar of Symbol.t
  | Uapp of t * t
  | Urecord of (Symbol.t * t) list
  | Uselect of t * Symbol.t
  | Ulet of pat * t * t
  | Ulet_fun of Symbol.t * closure * t
  | Udef of Symbol.t * closure * t
[@@deriving compare, sexp]

and closure = Uclosure of pat * t [@@deriving compare, sexp]
and pat = Upat_var of Symbol.t [@@deriving compare, sexp]
