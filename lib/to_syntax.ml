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

  let%expect_test "parse neg int" =
    run_it "-1";
    [%expect
      {| (Uint(value -1)(span(Span(filename"")(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 2)))))) |}]

  let%expect_test "parse float" =
    run_it "3.14";
    [%expect
      {| (Ufloat(value 3.14)(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 4)))))) |}]

  let%expect_test "parse var" =
    run_it "x";
    [%expect
      {| (Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))) |}]

  let%expect_test "parse app" =
    run_it "f x";
    [%expect
      {| (Uapp(fn(Uvar(value(Symbol f))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 2)))(finish(Position(line 1)(column 3)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 3)))))) |}]

  let%expect_test "no empty records" =
    run_it "{}";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

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
      {| (Ulet(binding(Symbol x))(is_mutable false)(value(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 9)))))))(app(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 13)))(finish(Position(line 1)(column 14)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 14)))))) |}]

  let%expect_test "incomplete let" =
    run_it "let x = 1";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

  let%expect_test "let closure" =
    run_it "let f x -> x in f 1";
    [%expect
      {| (Ulet_fun(name(Symbol f))(closure(Uclosure(parameter(Symbol x))(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 11)))(finish(Position(line 1)(column 12)))))))(span(Span(filename"")(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 11)))))))(app(Uapp(fn(Uvar(value(Symbol f))(span(Span(filename"")(start(Position(line 1)(column 16)))(finish(Position(line 1)(column 17)))))))(value(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 18)))(finish(Position(line 1)(column 19)))))))(span(Span(filename"")(start(Position(line 1)(column 16)))(finish(Position(line 1)(column 19)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 19)))))) |}]

  let%expect_test "closure" =
    run_it "fn x -> x";
    [%expect
      {| (Ulambda(closure(Uclosure(parameter(Symbol x))(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 9)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 9)))))))) |}]

  let%expect_test "def" =
    run_it "def f x = x in f 0";
    [%expect
      {| (Udef(name(Symbol f))(closure(Uclosure(parameter(Symbol x))(value(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 10)))(finish(Position(line 1)(column 11)))))))(span(Span(filename"")(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 10)))))))(app(Uapp(fn(Uvar(value(Symbol f))(span(Span(filename"")(start(Position(line 1)(column 15)))(finish(Position(line 1)(column 16)))))))(value(Uint(value 0)(span(Span(filename"")(start(Position(line 1)(column 17)))(finish(Position(line 1)(column 18)))))))(span(Span(filename"")(start(Position(line 1)(column 15)))(finish(Position(line 1)(column 18)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 18)))))) |}]

  let%expect_test "bad def" =
    run_it "def f = x in f 0";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

  let%expect_test "binop" =
    run_it "1 + 2";
    [%expect
      {| (Uop(op Add)(left(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(right(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 5)))))) |}]

  let%expect_test "binop precedence" =
    run_it "1 + 2 * 3";
    [%expect
      {| (Uop(op Add)(left(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(right(Uop(op Mul)(left(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5)))))))(right(Uint(value 3)(span(Span(filename"")(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 9)))))))(span(Span(filename"")(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 9)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 9)))))) |}]

  let%expect_test "relop" =
    run_it "1 < 2";
    [%expect
      {| (Uop(op Lt)(left(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(right(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 5)))))) |}]

  let%expect_test "bool" =
    run_it "true";
    [%expect
      {| (Ubool(value true)(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 4)))))) |}]

  let%expect_test "binop paren" =
    run_it "(1 + 2) * 3";
    [%expect
      {| (Uop(op Mul)(left(Utuple(values((Uop(op Add)(left(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 2)))))))(right(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 5)))(finish(Position(line 1)(column 6)))))))(span(Span(filename"")(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 6))))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 7)))))))(right(Uint(value 3)(span(Span(filename"")(start(Position(line 1)(column 10)))(finish(Position(line 1)(column 11)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 11)))))) |}]

  let%expect_test "if end" =
    run_it "if 1 < 2 then 2 end";
    [%expect
      {| (Uif_end(cond(Uop(op Lt)(left(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))(right(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 7)))(finish(Position(line 1)(column 8)))))))(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 8)))))))(then_(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 14)))(finish(Position(line 1)(column 15)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 19)))))) |}]

  let%expect_test "if" =
    run_it "if 1 < 2 then 2 else 3";
    [%expect
      {| (Uif(cond(Uop(op Lt)(left(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))(right(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 7)))(finish(Position(line 1)(column 8)))))))(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 8)))))))(then_(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 14)))(finish(Position(line 1)(column 15)))))))(else_(Uint(value 3)(span(Span(filename"")(start(Position(line 1)(column 21)))(finish(Position(line 1)(column 22)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 22)))))) |}]

  let%expect_test "unit" =
    run_it "()";
    [%expect
      {| (Utuple(values())(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 2)))))) |}]

  let%expect_test "paren" =
    run_it "(1)";
    [%expect
      {| (Utuple(values((Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 2))))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 3)))))) |}]

  let%expect_test "tuple" =
    run_it "(1, 2)";
    [%expect
      {| (Utuple(values((Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 2))))))(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5))))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 6)))))) |}]

  let%expect_test "tuple single" =
    (* TODO: interpret as box[T] *)
    run_it "(1,)";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

  let%expect_test "tuple index" =
    run_it "(1, 2).0";
    [%expect
      {| (Utuple_subscript(value(Utuple(values((Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 2))))))(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5))))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 6)))))))(index 0)(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 8)))))) |}]

  let%expect_test "empty vector" =
    run_it "[||]";
    [%expect
      {| (Uvector(values())(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 4)))))) |}]

  let%expect_test "vector" =
    run_it "[| 1, 2 |]";
    [%expect
      {| (Uvector(values((Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4))))))(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 7))))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 10)))))) |}]

  let%expect_test "heterogeneous vector" =
    run_it "[| 1, true |]";
    [%expect
      {| (Uvector(values((Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4))))))(Ubool(value true)(span(Span(filename"")(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 10))))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 13)))))) |}]

  let%expect_test "vector subscript" =
    run_it "[| 1, 2 |][0]";
    [%expect
      {| (Usubscript(value(Uvector(values((Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4))))))(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 7))))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 10)))))))(index(Uint(value 0)(span(Span(filename"")(start(Position(line 1)(column 11)))(finish(Position(line 1)(column 12)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 13)))))) |}]

  let%expect_test "let mut" =
    run_it "let mut x = 1 in x";
    [%expect
      {| (Ulet(binding(Symbol x))(is_mutable true)(value(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 12)))(finish(Position(line 1)(column 13)))))))(app(Uvar(value(Symbol x))(span(Span(filename"")(start(Position(line 1)(column 17)))(finish(Position(line 1)(column 18)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 18)))))) |}]

  let%expect_test "mut update" =
    run_it "x = 1";
    [%expect
      {| (Uassign(name(Symbol x))(value(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 5)))))) |}]

  let%expect_test "vector update" =
    run_it "xs[0] = 1";
    [%expect
      {| (Uassign_subscript(value(Uvar(value(Symbol xs))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 2)))))))(index(Uint(value 0)(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))(new_value(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 9)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 9)))))) |}]

  let%expect_test "seq" =
    run_it "1; 2";
    [%expect
      {| (Useq(first(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(second(Uint(value 2)(span(Span(filename"")(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 4)))))) |}]

  let%expect_test "let mut update" =
    run_it "let mut a = 0 in a = 1";
    [%expect
      {| (Ulet(binding(Symbol a))(is_mutable true)(value(Uint(value 0)(span(Span(filename"")(start(Position(line 1)(column 12)))(finish(Position(line 1)(column 13)))))))(app(Uassign(name(Symbol a))(value(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 21)))(finish(Position(line 1)(column 22)))))))(span(Span(filename"")(start(Position(line 1)(column 17)))(finish(Position(line 1)(column 22)))))))(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 22)))))) |}]

  let%expect_test "forever" =
    run_it {| for do () end |};
    [%expect
      {| (Ufor(iterates Udone)(body(Utuple(values())(span(Span(filename"")(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 10)))))))(span(Span(filename"")(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 14)))))) |}]

  let%expect_test "counter loop" =
    run_it
      {| 
      let mut i = 0 in
      for do
        i = i + 1
      end;
      i
    |};
    [%expect
      {| (Ulet(binding(Symbol i))(is_mutable true)(value(Uint(value 0)(span(Span(filename"")(start(Position(line 1)(column 20)))(finish(Position(line 1)(column 21)))))))(app(Useq(first(Ufor(iterates Udone)(body(Uassign(name(Symbol i))(value(Uop(op Add)(left(Uvar(value(Symbol i))(span(Span(filename"")(start(Position(line 1)(column 50)))(finish(Position(line 1)(column 51)))))))(right(Uint(value 1)(span(Span(filename"")(start(Position(line 1)(column 54)))(finish(Position(line 1)(column 55)))))))(span(Span(filename"")(start(Position(line 1)(column 50)))(finish(Position(line 1)(column 55)))))))(span(Span(filename"")(start(Position(line 1)(column 46)))(finish(Position(line 1)(column 55)))))))(span(Span(filename"")(start(Position(line 1)(column 31)))(finish(Position(line 1)(column 65)))))))(second(Uvar(value(Symbol i))(span(Span(filename"")(start(Position(line 1)(column 73)))(finish(Position(line 1)(column 74)))))))(span(Span(filename"")(start(Position(line 1)(column 31)))(finish(Position(line 1)(column 74)))))))(span(Span(filename"")(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 74)))))) |}]

  let%expect_test "for range" =
    run_it
      {| 
        let f x -> () in
        for i <- 0 to 10 do
          f i
        end
        |};
    [%expect
      {| (Ulet_fun(name(Symbol f))(closure(Uclosure(parameter(Symbol x))(value(Utuple(values())(span(Span(filename"")(start(Position(line 1)(column 21)))(finish(Position(line 1)(column 23)))))))(span(Span(filename"")(start(Position(line 1)(column 14)))(finish(Position(line 1)(column 21)))))))(app(Ufor(iterates(Uiterate(name(Symbol i))(start(Uint(value 0)(span(Span(filename"")(start(Position(line 1)(column 44)))(finish(Position(line 1)(column 45)))))))(finish(Uint(value 10)(span(Span(filename"")(start(Position(line 1)(column 49)))(finish(Position(line 1)(column 51)))))))(is_ascending true)(span(Span(filename"")(start(Position(line 1)(column 39)))(finish(Position(line 1)(column 51)))))(rest Udone)))(body(Uapp(fn(Uvar(value(Symbol f))(span(Span(filename"")(start(Position(line 1)(column 65)))(finish(Position(line 1)(column 66)))))))(value(Uvar(value(Symbol i))(span(Span(filename"")(start(Position(line 1)(column 67)))(finish(Position(line 1)(column 68)))))))(span(Span(filename"")(start(Position(line 1)(column 65)))(finish(Position(line 1)(column 68)))))))(span(Span(filename"")(start(Position(line 1)(column 35)))(finish(Position(line 1)(column 80)))))))(span(Span(filename"")(start(Position(line 1)(column 10)))(finish(Position(line 1)(column 80)))))) |}]

  let%expect_test "for nested loop" =
    run_it
      {| 
        for 
          i <- 0 to 10,
          j <- 0 to 10
        do
          let x = i + j in
          ()
        end
        |};
    [%expect
      {| (Ufor(iterates(Uiterate(name(Symbol i))(start(Uint(value 0)(span(Span(filename"")(start(Position(line 1)(column 30)))(finish(Position(line 1)(column 31)))))))(finish(Uint(value 10)(span(Span(filename"")(start(Position(line 1)(column 35)))(finish(Position(line 1)(column 37)))))))(is_ascending true)(span(Span(filename"")(start(Position(line 1)(column 25)))(finish(Position(line 1)(column 37)))))(rest(Uiterate(name(Symbol j))(start(Uint(value 0)(span(Span(filename"")(start(Position(line 1)(column 54)))(finish(Position(line 1)(column 55)))))))(finish(Uint(value 10)(span(Span(filename"")(start(Position(line 1)(column 59)))(finish(Position(line 1)(column 61)))))))(is_ascending true)(span(Span(filename"")(start(Position(line 1)(column 49)))(finish(Position(line 1)(column 61)))))(rest Udone)))))(body(Ulet(binding(Symbol x))(is_mutable false)(value(Uop(op Add)(left(Uvar(value(Symbol i))(span(Span(filename"")(start(Position(line 1)(column 91)))(finish(Position(line 1)(column 92)))))))(right(Uvar(value(Symbol j))(span(Span(filename"")(start(Position(line 1)(column 95)))(finish(Position(line 1)(column 96)))))))(span(Span(filename"")(start(Position(line 1)(column 91)))(finish(Position(line 1)(column 96)))))))(app(Utuple(values())(span(Span(filename"")(start(Position(line 1)(column 110)))(finish(Position(line 1)(column 112)))))))(span(Span(filename"")(start(Position(line 1)(column 83)))(finish(Position(line 1)(column 112)))))))(span(Span(filename"")(start(Position(line 1)(column 10)))(finish(Position(line 1)(column 124)))))) |}]

  let%expect_test "for decreasing loop" =
    run_it
      {| 
        for 
          i <- 0 downto 10
        do
          ()
        end
        |};
    [%expect
      {| (Ufor(iterates(Uiterate(name(Symbol i))(start(Uint(value 0)(span(Span(filename"")(start(Position(line 1)(column 30)))(finish(Position(line 1)(column 31)))))))(finish(Uint(value 10)(span(Span(filename"")(start(Position(line 1)(column 39)))(finish(Position(line 1)(column 41)))))))(is_ascending false)(span(Span(filename"")(start(Position(line 1)(column 25)))(finish(Position(line 1)(column 41)))))(rest Udone)))(body(Utuple(values())(span(Span(filename"")(start(Position(line 1)(column 63)))(finish(Position(line 1)(column 65)))))))(span(Span(filename"")(start(Position(line 1)(column 10)))(finish(Position(line 1)(column 77)))))) |}]
end
