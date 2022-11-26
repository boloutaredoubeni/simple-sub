module Primop = struct
  type t =
    | Add
    | Sub
    | Mul
    | Div
    | Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
    | And
    | Or
    | Not
end

module Symbol = struct
  type t = Symbol of string

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

and closure = Uclosure of pat * t
and pat = Upat_var of Symbol.t
