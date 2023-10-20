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
      {| (Utuple(values())(span(Span(filename"")(start(Position(line 0)(column 0)))(finish(Position(line 0)(column 0)))))) |}]

  let%expect_test "parse int" =
    run_it "1";
    [%expect
      {| (Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))) |}]

  let%expect_test "parse neg int" =
    run_it "-1";
    [%expect
      {| (Uint(value -1)(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 2)))))) |}]

  let%expect_test "parse float" =
    run_it "3.14";
    [%expect
      {| (Ufloat(value 3.14)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 4)))))) |}]

  let%expect_test "parse var" =
    run_it "x";
    [%expect
      {| (Uvar(value(Symbol x))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))) |}]

  let%expect_test "parse app" =
    run_it "f x";
    [%expect
      {| (Uapp(fn(Uvar(value(Symbol f))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(value(Uvar(value(Symbol x))(span(Span(filename stdin)(start(Position(line 1)(column 2)))(finish(Position(line 1)(column 3)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 3)))))) |}]

  let%expect_test "no empty records" =
    run_it "{}";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

  let%expect_test "record" =
    run_it "{x=1}";
    [%expect
      {| (Urecord(proto())(fields(((Symbol x)(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 5)))))) |}]

  let%expect_test "field select" =
    run_it "x.y";
    [%expect
      {| (Uselect(value(Uvar(value(Symbol x))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(field(Symbol y))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 3)))))) |}]

  let%expect_test "let" =
    run_it "let x = 1 in x";
    [%expect
      {| (Ulet(binding(Symbol x))(mutability Immutable)(value(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 9)))))))(app(Uvar(value(Symbol x))(span(Span(filename stdin)(start(Position(line 1)(column 13)))(finish(Position(line 1)(column 14)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 14)))))) |}]

  let%expect_test "incomplete let" =
    run_it "let x = 1";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

  let%expect_test "let closure" =
    run_it "let f x -> x in f 1";
    [%expect
      {| (Ulet_fun(name(Symbol f))(closure(Uclosure(parameter(Symbol x))(value(Uvar(value(Symbol x))(span(Span(filename stdin)(start(Position(line 1)(column 11)))(finish(Position(line 1)(column 12)))))))(span(Span(filename stdin)(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 11)))))))(app(Uapp(fn(Uvar(value(Symbol f))(span(Span(filename stdin)(start(Position(line 1)(column 16)))(finish(Position(line 1)(column 17)))))))(value(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 18)))(finish(Position(line 1)(column 19)))))))(span(Span(filename stdin)(start(Position(line 1)(column 16)))(finish(Position(line 1)(column 19)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 19)))))) |}]

  let%expect_test "closure" =
    run_it "fn x -> x end";
    [%expect
      {| (Ulambda(closure(Uclosure(parameter(Symbol x))(value(Uvar(value(Symbol x))(span(Span(filename stdin)(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 9)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 13)))))))) |}]

  let%expect_test "def" =
    run_it "def f x = x in f 0";
    [%expect
      {| (Udef(name(Symbol f))(closure(Uclosure(parameter(Symbol x))(value(Uvar(value(Symbol x))(span(Span(filename stdin)(start(Position(line 1)(column 10)))(finish(Position(line 1)(column 11)))))))(span(Span(filename stdin)(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 10)))))))(app(Uapp(fn(Uvar(value(Symbol f))(span(Span(filename stdin)(start(Position(line 1)(column 15)))(finish(Position(line 1)(column 16)))))))(value(Uint(value 0)(span(Span(filename stdin)(start(Position(line 1)(column 17)))(finish(Position(line 1)(column 18)))))))(span(Span(filename stdin)(start(Position(line 1)(column 15)))(finish(Position(line 1)(column 18)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 18)))))) |}]

  let%expect_test "bad def" =
    run_it "def f = x in f 0";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

  let%expect_test "binop" =
    run_it "1 + 2";
    [%expect
      {| (Uop(op Add)(left(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(right(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 5)))))) |}]

  let%expect_test "binop precedence" =
    run_it "1 + 2 * 3";
    [%expect
      {| (Uop(op Add)(left(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(right(Uop(op Mul)(left(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5)))))))(right(Uint(value 3)(span(Span(filename stdin)(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 9)))))))(span(Span(filename stdin)(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 9)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 9)))))) |}]

  let%expect_test "relop" =
    run_it "1 < 2";
    [%expect
      {| (Uop(op Lt)(left(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(right(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 5)))))) |}]

  let%expect_test "bool" =
    run_it "true";
    [%expect
      {| (Ubool(value true)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 4)))))) |}]

  let%expect_test "binop paren" =
    run_it "(1 + 2) * 3";
    [%expect
      {| (Uop(op Mul)(left(Utuple(values((Uop(op Add)(left(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 2)))))))(right(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 5)))(finish(Position(line 1)(column 6)))))))(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 6))))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 7)))))))(right(Uint(value 3)(span(Span(filename stdin)(start(Position(line 1)(column 10)))(finish(Position(line 1)(column 11)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 11)))))) |}]

  let%expect_test "if end" =
    run_it "if 1 < 2 then 2 end";
    [%expect
      {| (Uif_end(cond(Uop(op Lt)(left(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))(right(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 7)))(finish(Position(line 1)(column 8)))))))(span(Span(filename stdin)(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 8)))))))(then_(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 14)))(finish(Position(line 1)(column 15)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 19)))))) |}]

  let%expect_test "if" =
    run_it "if 1 < 2 then 2 else 3";
    [%expect
      {| (Uif(cond(Uop(op Lt)(left(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))(right(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 7)))(finish(Position(line 1)(column 8)))))))(span(Span(filename stdin)(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 8)))))))(then_(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 14)))(finish(Position(line 1)(column 15)))))))(else_(Uint(value 3)(span(Span(filename stdin)(start(Position(line 1)(column 21)))(finish(Position(line 1)(column 22)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 22)))))) |}]

  let%expect_test "unit" =
    run_it "()";
    [%expect
      {| (Utuple(values())(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 2)))))) |}]

  let%expect_test "paren" =
    run_it "(1)";
    [%expect
      {| (Utuple(values((Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 2))))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 3)))))) |}]

  let%expect_test "tuple" =
    run_it "(1, 2)";
    [%expect
      {| (Utuple(values((Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 2))))))(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5))))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 6)))))) |}]

  let%expect_test "record punning" =
    run_it "{x}";
    [%expect
      {| (Urecord(proto())(fields(((Symbol x)(Uvar(value(Symbol x))(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 2)))))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 3)))))) |}]

  let%expect_test "tuple single" =
    (* TODO: interpret as box[T] *)
    run_it "(1,)";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

  let%expect_test "tuple index" =
    run_it "(1, 2).0";
    [%expect
      {| (Utuple_subscript(value(Utuple(values((Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 2))))))(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5))))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 6)))))))(index 0)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 8)))))) |}]

  let%expect_test "empty vector" =
    run_it "[||]";
    [%expect
      {| (Uvector(values())(mutability Immutable)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 4)))))) |}]

  let%expect_test "vector" =
    run_it "[| 1, 2 |]";
    [%expect
      {| (Uvector(values((Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4))))))(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 7))))))))(mutability Immutable)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 10)))))) |}]

  let%expect_test "union vector" =
    run_it "[| 1, true |]";
    [%expect
      {| (Uvector(values((Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4))))))(Ubool(value true)(span(Span(filename stdin)(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 10))))))))(mutability Immutable)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 13)))))) |}]

  let%expect_test "vector subscript" =
    run_it {| let xs = [| 1, 2 |] in xs[0] |};
    [%expect
      {| (Ulet(binding(Symbol xs))(mutability Immutable)(value(Uvector(values((Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 13)))(finish(Position(line 1)(column 14))))))(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 16)))(finish(Position(line 1)(column 17))))))))(mutability Immutable)(span(Span(filename stdin)(start(Position(line 1)(column 10)))(finish(Position(line 1)(column 20)))))))(app(Usubscript(value(Symbol xs))(index(Uint(value 0)(span(Span(filename stdin)(start(Position(line 1)(column 27)))(finish(Position(line 1)(column 28)))))))(span(Span(filename stdin)(start(Position(line 1)(column 24)))(finish(Position(line 1)(column 29)))))))(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 29)))))) |}]

  let%expect_test "local readwrite vector" =
    run_it {|
       let xs = mut [| 0, 1|] in
       xs[0] = 1 |};
    [%expect
      {| (Ulet(binding(Symbol xs))(mutability Immutable)(value(Uvector(values((Uint(value 0)(span(Span(filename stdin)(start(Position(line 2)(column 23)))(finish(Position(line 2)(column 24))))))(Uint(value 1)(span(Span(filename stdin)(start(Position(line 2)(column 26)))(finish(Position(line 2)(column 27))))))))(mutability Mutable)(span(Span(filename stdin)(start(Position(line 2)(column 16)))(finish(Position(line 2)(column 29)))))))(app(Uassign_subscript(name(Symbol xs))(index(Uint(value 0)(span(Span(filename stdin)(start(Position(line 3)(column 10)))(finish(Position(line 3)(column 11)))))))(value(Uint(value 1)(span(Span(filename stdin)(start(Position(line 3)(column 15)))(finish(Position(line 3)(column 16)))))))(span(Span(filename stdin)(start(Position(line 3)(column 7)))(finish(Position(line 3)(column 16)))))))(span(Span(filename stdin)(start(Position(line 2)(column 7)))(finish(Position(line 3)(column 16)))))) |}]

  let%expect_test "readwrite vector, no passing" =
    run_it {|
       let xs = ref [| 0, 1|] in
       xs[0] = 1 |};
    [%expect
      {| (Ulet(binding(Symbol xs))(mutability Immutable)(value(Uvector(values((Uint(value 0)(span(Span(filename stdin)(start(Position(line 2)(column 23)))(finish(Position(line 2)(column 24))))))(Uint(value 1)(span(Span(filename stdin)(start(Position(line 2)(column 26)))(finish(Position(line 2)(column 27))))))))(mutability Reference)(span(Span(filename stdin)(start(Position(line 2)(column 16)))(finish(Position(line 2)(column 29)))))))(app(Uassign_subscript(name(Symbol xs))(index(Uint(value 0)(span(Span(filename stdin)(start(Position(line 3)(column 10)))(finish(Position(line 3)(column 11)))))))(value(Uint(value 1)(span(Span(filename stdin)(start(Position(line 3)(column 15)))(finish(Position(line 3)(column 16)))))))(span(Span(filename stdin)(start(Position(line 3)(column 7)))(finish(Position(line 3)(column 16)))))))(span(Span(filename stdin)(start(Position(line 2)(column 7)))(finish(Position(line 3)(column 16)))))) |}]

  let%expect_test "borrow readonly vector" =
    run_it "xs[..]";
    [%expect
      {| (Uslice(value(Symbol xs))(readability Readonly)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 6)))))) |}]

  let%expect_test "borrow readwrite vector" =
    run_it "&mut xs[..]";
    [%expect
      {| (Uslice(value(Symbol xs))(readability ReadWrite)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 11)))))) |}]

  let%expect_test "let mut" =
    run_it "let mut x = 1 in x";
    [%expect
      {| (Ulet(binding(Symbol x))(mutability Mutable)(value(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 12)))(finish(Position(line 1)(column 13)))))))(app(Uvar(value(Symbol x))(span(Span(filename stdin)(start(Position(line 1)(column 17)))(finish(Position(line 1)(column 18)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 18)))))) |}]

  let%expect_test "mut update" =
    run_it "x = 1";
    [%expect
      {| (Uassign(name(Symbol x))(value(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 4)))(finish(Position(line 1)(column 5)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 5)))))) |}]

  let%expect_test "vector update" =
    run_it "xs[0] = 1";
    [%expect
      {| (Uassign_subscript(name(Symbol xs))(index(Uint(value 0)(span(Span(filename stdin)(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))(value(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 9)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 9)))))) |}]

  let%expect_test "seq" =
    run_it "1; 2";
    [%expect
      {| (Useq(first(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 1)))))))(second(Uint(value 2)(span(Span(filename stdin)(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 4)))))) |}]

  let%expect_test "let mut update" =
    run_it "let mut a = 0 in a = 1";
    [%expect
      {| (Ulet(binding(Symbol a))(mutability Mutable)(value(Uint(value 0)(span(Span(filename stdin)(start(Position(line 1)(column 12)))(finish(Position(line 1)(column 13)))))))(app(Uassign(name(Symbol a))(value(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 21)))(finish(Position(line 1)(column 22)))))))(span(Span(filename stdin)(start(Position(line 1)(column 17)))(finish(Position(line 1)(column 22)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 22)))))) |}]

  let%expect_test "let ref" =
    run_it "let ref a = 0 in a";
    [%expect
      {| (Ulet(binding(Symbol a))(mutability Reference)(value(Uint(value 0)(span(Span(filename stdin)(start(Position(line 1)(column 12)))(finish(Position(line 1)(column 13)))))))(app(Uvar(value(Symbol a))(span(Span(filename stdin)(start(Position(line 1)(column 17)))(finish(Position(line 1)(column 18)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 18)))))) |}]

  let%expect_test "ref update" =
    run_it "a := 1";
    [%expect
      {| (Uupdate_ref(name(Symbol a))(value(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 5)))(finish(Position(line 1)(column 6)))))))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 6)))))) |}]

  let%expect_test "deref" =
    run_it "!a";
    [%expect
      {| (Uderef(name(Symbol a))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 2)))))) |}]

  let%expect_test "global readwrite reference" =
    run_it {|
       let ref mut xs = 0 in
       xs := 1 |};
    [%expect
      {| (Ulet(binding(Symbol xs))(mutability MutableReference)(value(Uint(value 0)(span(Span(filename stdin)(start(Position(line 2)(column 24)))(finish(Position(line 2)(column 25)))))))(app(Uupdate_ref(name(Symbol xs))(value(Uint(value 1)(span(Span(filename stdin)(start(Position(line 3)(column 13)))(finish(Position(line 3)(column 14)))))))(span(Span(filename stdin)(start(Position(line 3)(column 7)))(finish(Position(line 3)(column 14)))))))(span(Span(filename stdin)(start(Position(line 2)(column 7)))(finish(Position(line 3)(column 14)))))) |}]

  let%expect_test "borrow readwrite reference" =
    run_it "&mut r";
    [%expect
      {| (Umutable_ref(value(Symbol r))(span(Span(filename stdin)(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 6)))))) |}]

  let%expect_test "forever" =
    run_it {| for do () end |};
    [%expect
      {| (Ufor(iterates Udone)(body(Utuple(values())(span(Span(filename stdin)(start(Position(line 1)(column 8)))(finish(Position(line 1)(column 10)))))))(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 14)))))) |}]

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
      {| (Ulet(binding(Symbol i))(mutability Mutable)(value(Uint(value 0)(span(Span(filename stdin)(start(Position(line 2)(column 18)))(finish(Position(line 2)(column 19)))))))(app(Useq(first(Ufor(iterates Udone)(body(Uassign(name(Symbol i))(value(Uop(op Add)(left(Uvar(value(Symbol i))(span(Span(filename stdin)(start(Position(line 4)(column 12)))(finish(Position(line 4)(column 13)))))))(right(Uint(value 1)(span(Span(filename stdin)(start(Position(line 4)(column 16)))(finish(Position(line 4)(column 17)))))))(span(Span(filename stdin)(start(Position(line 4)(column 12)))(finish(Position(line 4)(column 17)))))))(span(Span(filename stdin)(start(Position(line 4)(column 8)))(finish(Position(line 4)(column 17)))))))(span(Span(filename stdin)(start(Position(line 3)(column 6)))(finish(Position(line 5)(column 9)))))))(second(Uvar(value(Symbol i))(span(Span(filename stdin)(start(Position(line 6)(column 6)))(finish(Position(line 6)(column 7)))))))(span(Span(filename stdin)(start(Position(line 3)(column 6)))(finish(Position(line 6)(column 7)))))))(span(Span(filename stdin)(start(Position(line 2)(column 6)))(finish(Position(line 6)(column 7)))))) |}]

  let%expect_test "for range" =
    run_it
      {| 
        let f x -> () in
        for i <- 0 to 10 do
          f i
        end
        |};
    [%expect
      {| (Ulet_fun(name(Symbol f))(closure(Uclosure(parameter(Symbol x))(value(Utuple(values())(span(Span(filename stdin)(start(Position(line 2)(column 19)))(finish(Position(line 2)(column 21)))))))(span(Span(filename stdin)(start(Position(line 2)(column 12)))(finish(Position(line 2)(column 19)))))))(app(Ufor(iterates(Uiterate(name(Symbol i))(start(Uint(value 0)(span(Span(filename stdin)(start(Position(line 3)(column 17)))(finish(Position(line 3)(column 18)))))))(finish(Uint(value 10)(span(Span(filename stdin)(start(Position(line 3)(column 22)))(finish(Position(line 3)(column 24)))))))(is_ascending true)(span(Span(filename stdin)(start(Position(line 3)(column 12)))(finish(Position(line 3)(column 24)))))(rest Udone)))(body(Uapp(fn(Uvar(value(Symbol f))(span(Span(filename stdin)(start(Position(line 4)(column 10)))(finish(Position(line 4)(column 11)))))))(value(Uvar(value(Symbol i))(span(Span(filename stdin)(start(Position(line 4)(column 12)))(finish(Position(line 4)(column 13)))))))(span(Span(filename stdin)(start(Position(line 4)(column 10)))(finish(Position(line 4)(column 13)))))))(span(Span(filename stdin)(start(Position(line 3)(column 8)))(finish(Position(line 5)(column 11)))))))(span(Span(filename stdin)(start(Position(line 2)(column 8)))(finish(Position(line 5)(column 11)))))) |}]

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
      {| (Ufor(iterates(Uiterate(name(Symbol i))(start(Uint(value 0)(span(Span(filename stdin)(start(Position(line 3)(column 15)))(finish(Position(line 3)(column 16)))))))(finish(Uint(value 10)(span(Span(filename stdin)(start(Position(line 3)(column 20)))(finish(Position(line 3)(column 22)))))))(is_ascending true)(span(Span(filename stdin)(start(Position(line 3)(column 10)))(finish(Position(line 3)(column 22)))))(rest(Uiterate(name(Symbol j))(start(Uint(value 0)(span(Span(filename stdin)(start(Position(line 4)(column 15)))(finish(Position(line 4)(column 16)))))))(finish(Uint(value 10)(span(Span(filename stdin)(start(Position(line 4)(column 20)))(finish(Position(line 4)(column 22)))))))(is_ascending true)(span(Span(filename stdin)(start(Position(line 4)(column 10)))(finish(Position(line 4)(column 22)))))(rest Udone)))))(body(Ulet(binding(Symbol x))(mutability Immutable)(value(Uop(op Add)(left(Uvar(value(Symbol i))(span(Span(filename stdin)(start(Position(line 6)(column 18)))(finish(Position(line 6)(column 19)))))))(right(Uvar(value(Symbol j))(span(Span(filename stdin)(start(Position(line 6)(column 22)))(finish(Position(line 6)(column 23)))))))(span(Span(filename stdin)(start(Position(line 6)(column 18)))(finish(Position(line 6)(column 23)))))))(app(Utuple(values())(span(Span(filename stdin)(start(Position(line 7)(column 10)))(finish(Position(line 7)(column 12)))))))(span(Span(filename stdin)(start(Position(line 6)(column 10)))(finish(Position(line 7)(column 12)))))))(span(Span(filename stdin)(start(Position(line 2)(column 8)))(finish(Position(line 8)(column 11)))))) |}]

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
      {| (Ufor(iterates(Uiterate(name(Symbol i))(start(Uint(value 0)(span(Span(filename stdin)(start(Position(line 3)(column 15)))(finish(Position(line 3)(column 16)))))))(finish(Uint(value 10)(span(Span(filename stdin)(start(Position(line 3)(column 24)))(finish(Position(line 3)(column 26)))))))(is_ascending false)(span(Span(filename stdin)(start(Position(line 3)(column 10)))(finish(Position(line 3)(column 26)))))(rest Udone)))(body(Utuple(values())(span(Span(filename stdin)(start(Position(line 5)(column 10)))(finish(Position(line 5)(column 12)))))))(span(Span(filename stdin)(start(Position(line 2)(column 8)))(finish(Position(line 6)(column 11)))))) |}]

  let%expect_test "empty char" =
    run_it {| '' |};
    [%expect {| ("Fx__Text.Syntax_error(\"stdin\", \"\", _)") |}]

  let%expect_test "chars are not strings" =
    run_it {| 'hello world' |};
    [%expect {| ("Fx__Text.Syntax_error(\"stdin\", \"e\", _)") |}]

  let%expect_test "a char" =
    run_it {| 'a' |};
    [%expect
      {| (Uchar(value a)(span(Span(filename stdin)(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))) |}]

  let%expect_test "escape char" =
    run_it {|'\n' |};
    [%expect
      {| (Uchar(value"\n")(span(Span(filename stdin)(start(Position(line 1)(column 3)))(finish(Position(line 1)(column 4)))))) |}]

  let%expect_test "emoji" =
    run_it {|"😀" |};
    [%expect
      {| (Ustring(value"\240\159\152\128")(span(Span(filename stdin)(start(Position(line 1)(column 5)))(finish(Position(line 1)(column 6)))))) |}]

  let%expect_test "unterminated char" =
    run_it {|'  |};
    [%expect {| ("Fx__Text.Syntax_error(\"stdin\", \" \", _)") |}]

  let%expect_test "unterminated strings" =
    run_it {| "hello world |};
    [%expect {| ("Fx__Text.Syntax_error(\"stdin\", \"\", _)") |}]

  let%expect_test "strings" =
    run_it {| "hello world" |};
    [%expect
      {| (Ustring(value"hello world")(span(Span(filename stdin)(start(Position(line 1)(column 13)))(finish(Position(line 1)(column 14)))))) |}]

  let%expect_test "string concat" =
    run_it {| "hello" ^ "world" |};
    [%expect
      {| (Uop(op SAdd)(left(Ustring(value hello)(span(Span(filename stdin)(start(Position(line 1)(column 7)))(finish(Position(line 1)(column 8)))))))(right(Ustring(value world)(span(Span(filename stdin)(start(Position(line 1)(column 17)))(finish(Position(line 1)(column 18)))))))(span(Span(filename stdin)(start(Position(line 1)(column 7)))(finish(Position(line 1)(column 18)))))) |}]

  let%expect_test "iife" =
    run_it {| (fn x -> x end) 0 |};
    [%expect
      {| (Uapp(fn(Utuple(values((Ulambda(closure(Uclosure(parameter(Symbol x))(value(Uvar(value(Symbol x))(span(Span(filename stdin)(start(Position(line 1)(column 10)))(finish(Position(line 1)(column 11)))))))(span(Span(filename stdin)(start(Position(line 1)(column 2)))(finish(Position(line 1)(column 15))))))))))(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 16)))))))(value(Uint(value 0)(span(Span(filename stdin)(start(Position(line 1)(column 17)))(finish(Position(line 1)(column 18)))))))(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 18)))))) |}]

  let%expect_test "cases" =
    run_it {| Some 1 |};
    [%expect
      {| (Ucase(case(Symbol Some))(value(Uint(value 1)(span(Span(filename stdin)(start(Position(line 1)(column 6)))(finish(Position(line 1)(column 7)))))))(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 7)))))) |}]

  let%expect_test "unit case" =
    run_it {| None |};
    [%expect
      {| (Ucase(case(Symbol None))(value(Utuple(values())(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 5)))))))(span(Span(filename stdin)(start(Position(line 1)(column 1)))(finish(Position(line 1)(column 5)))))) |}]

  let%expect_test "match" =
    run_it
      {| 
        match v 
          case Some x -> f x,
          case None -> ()
        end
    |};
    [%expect
      {| (Umatch(value(Uvar(value(Symbol v))(span(Span(filename stdin)(start(Position(line 2)(column 14)))(finish(Position(line 2)(column 15)))))))(cases(((Symbol Some)((Symbol x))(Uapp(fn(Uvar(value(Symbol f))(span(Span(filename stdin)(start(Position(line 3)(column 25)))(finish(Position(line 3)(column 26)))))))(value(Uvar(value(Symbol x))(span(Span(filename stdin)(start(Position(line 3)(column 27)))(finish(Position(line 3)(column 28)))))))(span(Span(filename stdin)(start(Position(line 3)(column 25)))(finish(Position(line 3)(column 28)))))))((Symbol None)()(Utuple(values())(span(Span(filename stdin)(start(Position(line 4)(column 23)))(finish(Position(line 4)(column 25)))))))))(span(Span(filename stdin)(start(Position(line 2)(column 8)))(finish(Position(line 5)(column 11)))))) |}]
end
