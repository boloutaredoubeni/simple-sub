open Core

let parse (s : string) : Syntax.t =
  s |> Lexing.from_string |> Parser.program Lexer.token
  |> Option.value ~default:Syntax.default

module Tests = struct
  let%expect_test "parse empty program" =
    parse "" |> Syntax.to_sexp_string |> print_endline;
    [%expect
      {| (Uint(value 0)(span(Span(filename"")(start(Position(line 0)(column 0)))(finish(Position(line 0)(column 0)))))) |}]

  let%expect_test "parse int" =
    parse "1" |> Syntax.to_sexp_string |> print_endline;
    [%expect
      {| (Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))) |}]

  let%expect_test "parse var" =
    parse "x" |> Syntax.to_sexp_string |> print_endline;
    [%expect
      {| (Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))) |}]

  let%expect_test "parse app" =
    parse "f x" |> Syntax.to_sexp_string |> print_endline;
    [%expect
      {| (Uapp(fn(Uvar(value(Symbol f))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 2)))(finish(Position(line 1)(column 3)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 3)))))) |}]

  let%expect_test "record" =
    parse "{x=1}" |> Syntax.to_sexp_string |> print_endline;
    [%expect
      {| (Urecord(fields(((Symbol x)(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 5)))))) |}]

  let%expect_test "field select" =
    parse "x.y" |> Syntax.to_sexp_string |> print_endline;
    [%expect
      {| (Uselect(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(field(Symbol y))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 3)))))) |}]

  let%expect_test "let" =
    parse "let x = 1 in x" |> Syntax.to_sexp_string |> print_endline;
    [%expect
      {| (Ulet(pattern(Upat_var(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5)))))))(value(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 9)))))))(app(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 13)))(finish(Position(line 1)(column 14)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 14)))))) |}]

  let%expect_test "let closure" =
    parse "let f x -> x in f 1" |> Syntax.to_sexp_string |> print_endline;
    [%expect
      {| (Ulet_fun(name(Symbol f))(closure(Uclosure(parameter(Upat_var(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 7)))))))(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 11)))(finish(Position(line 1)(column 12)))))))(span(Span(filename"")(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 11)))))))(app(Uapp(fn(Uvar(value(Symbol f))(span(Span(filename"")(start(Position(line 1)(column 16)))(finish(Position(line 1)(column 17)))))))(value(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 18)))(finish(Position(line 1)(column 19)))))))(span(Span(filename"")(start(Position(line 1)(column 16)))(finish(Position(line 1)(column 19)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 19)))))) |}]

  let%expect_test "def" =
    parse "def f x = x in f 0" |> Syntax.to_sexp_string |> print_endline;
    [%expect
      {| (Udef(name(Symbol f))(closure(Uclosure(parameter(Upat_var(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 7)))))))(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 10)))(finish(Position(line 1)(column 11)))))))(span(Span(filename"")(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 10)))))))(app(Uapp(fn(Uvar(value(Symbol f))(span(Span(filename"")(start(Position(line 1)(column 15)))(finish(Position(line 1)(column 16)))))))(value(Uint(value 0)(span(Span(filename"")(start(Position(line 1)(column 17)))(finish(Position(line 1)(column 18)))))))(span(Span(filename"")(start(Position(line 1)(column 15)))(finish(Position(line 1)(column 18)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 18)))))) |}]
end
