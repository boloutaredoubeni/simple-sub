open Core

module Position = struct
  type t = Position of { line : int; column : int }
  [@@deriving sexp, compare, equal]

  let create (line, column) = Position { line; column }

  let of_lexing_position (position : Lexing.position) =
    let line = position.pos_lnum in
    let column = position.pos_cnum - position.pos_bol in
    Position { line; column }

  let default = Position { line = 0; column = 0 }

  let to_string (Position { line; column }) =
    Printf.sprintf "line %d, column %d" line column

  module Tests = struct
    let%test_unit "default positions" = [%test_eq: t] default (create (0, 0))

    let%test_unit "different positions" =
      [%test_result: bool] (equal (create (0, 2)) (create (1, 2))) ~expect:false

    let%test_unit "compare positions" =
      [%test_result: int] (compare (create (0, 2)) (create (1, 2))) ~expect:(-1)
  end
end

module Span = struct
  type span =
    | Span of { filename : string; start : Position.t; finish : Position.t }
  [@@deriving sexp, compare, equal]

  let to_string (Span { filename; start; finish }) =
    Printf.sprintf "file %s, start %s, finish %s" filename
      (Position.to_string start)
      (Position.to_string finish)

  module type SPANNED = sig
    type t

    val span : t -> span
  end

  module Spanned : SPANNED = struct
    type t = span

    let span t = t
  end

  type t = span

  let create ((start, finish) : Lexing.position * Lexing.position) =
    let filename = start.pos_fname in
    let start = Position.of_lexing_position start in
    let finish = Position.of_lexing_position finish in
    Span { filename; start; finish }

  let default =
    Span { filename = ""; start = Position.default; finish = Position.default }

  let from_positions filename (start, finish) = Span { filename; start; finish }

  module Tests = struct
    let%test_unit "default span" =
      [%test_eq: span] default
        (from_positions "" (Position.default, Position.default))

    let%test_unit "different span" =
      [%test_result: bool]
        (equal_span
           (from_positions "a file" (Position.default, Position.default))
           (from_positions "another file" (Position.default, Position.default)))
        ~expect:false
  end
end

module Symbol = struct
  module T = struct
    type t = Symbol of string [@@deriving compare, sexp, equal]

    let of_string s = Symbol s
    let to_string (Symbol s) = s
  end

  include T
  include Comparable.Make (T)
end

module type FRESH_SYM = sig
  val f : unit -> Symbol.t
end

let create_fresh_sym () =
  let counter = ref 0 in
  (module struct
    let f () =
      let sym = Symbol.of_string (Printf.sprintf "__%d" !counter) in
      counter := !counter + 1;
      sym
  end : FRESH_SYM)
