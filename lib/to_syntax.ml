open Core

let parse (s : string) : Syntax.t Or_error.t =
  Or_error.try_with (fun () ->
      match s |> Lexing.from_string |> Parser.program Lexer.token with
      | Some program -> program
      | None -> Syntax.default)

module Tests = struct
  let run_it s =
    match
      parse s |> fun or_error ->
      Or_error.map or_error ~f:(fun ast ->
          Sexp.to_string (Syntax.sexp_of_t ast))
      |> Or_error.ok_exn |> print_endline
    with
    | () -> ()
    | exception exn -> print_endline (Exn.to_string exn)

  let%expect_test "parse empty program" =
    run_it "";
    [%expect
      {| (Uint(value 0)(span(Span(filename"")(start(Position(line 0)(column 0)))(finish(Position(line 0)(column 0)))))) |}]

  let%expect_test "parse int" =
    run_it "1";
    [%expect
      {| (Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))) |}]

  let%expect_test "parse var" =
    run_it "x";
    [%expect
      {| (Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))) |}]

  let%expect_test "parse app" =
    run_it "f x";
    [%expect
      {| (Uapp(fn(Uvar(value(Symbol f))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 2)))(finish(Position(line 1)(column 3)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 3)))))) |}]

  let%expect_test "record" =
    run_it "{x=1}";
    [%expect
      {| (Urecord(fields(((Symbol x)(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 5)))))) |}]

  let%expect_test "field select" =
    run_it "x.y";
    [%expect
      {| (Uselect(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(field(Symbol y))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 3)))))) |}]

  let%expect_test "let" =
    run_it "let x = 1 in x";
    [%expect
      {| (Ulet(pattern(Upat_var(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5)))))))(value(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 9)))))))(app(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 13)))(finish(Position(line 1)(column 14)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 14)))))) |}]

  let%expect_test "incomplete let" =
    run_it "let x = 1";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

  let%expect_test "let closure" =
    run_it "let f x -> x in f 1";
    [%expect
      {| (Ulet_fun(name(Symbol f))(closure(Uclosure(parameter(Upat_var(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 7)))))))(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 11)))(finish(Position(line 1)(column 12)))))))(span(Span(filename"")(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 11)))))))(app(Uapp(fn(Uvar(value(Symbol f))(span(Span(filename"")(start(Position(line 1)(column 16)))(finish(Position(line 1)(column 17)))))))(value(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 18)))(finish(Position(line 1)(column 19)))))))(span(Span(filename"")(start(Position(line 1)(column 16)))(finish(Position(line 1)(column 19)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 19)))))) |}]

  let%expect_test "closure" =
    run_it "fn x -> x";
    [%expect
      {| (Ulambda(closure(Uclosure(parameter(Upat_var(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 9)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 9)))))))) |}]

  let%expect_test "def" =
    run_it "def f x = x in f 0";
    [%expect
      {| (Udef(name(Symbol f))(closure(Uclosure(parameter(Upat_var(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 7)))))))(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 10)))(finish(Position(line 1)(column 11)))))))(span(Span(filename"")(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 10)))))))(app(Uapp(fn(Uvar(value(Symbol f))(span(Span(filename"")(start(Position(line 1)(column 15)))(finish(Position(line 1)(column 16)))))))(value(Uint(value 0)(span(Span(filename"")(start(Position(line 1)(column 17)))(finish(Position(line 1)(column 18)))))))(span(Span(filename"")(start(Position(line 1)(column 15)))(finish(Position(line 1)(column 18)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 18)))))) |}]

  let%expect_test "bad def" =
    run_it "def f = x in f 0";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]
end
