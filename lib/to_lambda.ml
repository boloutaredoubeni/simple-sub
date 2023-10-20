open Core
open Text

module type MAPPER = sig
  type self

  val create : unit -> self
  val map : Tast.t -> Lambda.t Or_error.t
  val map_ast : self -> Tast.t -> Lambda.t

  val map_closure :
    is_rec:Symbol.t option -> self -> Tast.Closure.t -> Lambda.Closure.t

  val map_type : self -> Tast.Simple_type.t -> Lambda.Type.t
  val map_primop : self -> Tast.Primop.t -> Lambda.Primop.t
  val map_iterate : self -> Tast.Iterate.t -> self * Lambda.Iterate.t
end

module Type_env = struct
  type t =
    | Self of {
        type_env : (Symbol.t, Lambda.Type.t, Symbol.comparator_witness) Map.t;
      }

  let create () = Self { type_env = Map.empty (module Symbol) }

  let add (Self { type_env }) ~key ~data =
    Self { type_env = Map.add_exn type_env ~key ~data }

  let find (Self { type_env }) key = Map.find type_env key
end

module Make (Fresh_sym : FRESH_SYM) : MAPPER = struct
  type self = Type_env.t

  let create () = Type_env.create ()

  let rec map t =
    let self = create () in
    Or_error.try_with (fun () -> map_ast self t)

  and map_ast self t =
    let open Tast in
    let open Lambda in
    match t with
    | Tint { value; span } -> Lint { value; span }
    | Tfloat { value; span } -> Lfloat { value; span }
    | Tbool { value; span } -> Lbool { value; span }
    | Tchar { value; span } -> Lchar { value; span }
    | Tunit { span } -> Lunit { span }
    | Tstring { value; span } -> Lstring { value; span }
    | Tvector { values; span; type' } ->
        let values = List.map values ~f:(map_ast self) in
        let type' = map_type self type' in
        Lvector { values; span; type' }
    | Ttuple { first; second; rest; span } ->
        let first = map_ast self first in
        let second = map_ast self second in
        let rest = List.map rest ~f:(map_ast self) in
        Ltuple { first; second; rest; span }
    | Ttuple_subscript { value; index; span; type' } ->
        let value = map_ast self value in
        let type' = map_type self type' in
        Ltuple_subscript { value; index; span; type' }
    | Tsubscript { value; index; span; type' } -> (
        match Type_env.find self value with
        | None -> failwith "unbound var"
        | Some _ ->
            let index = map_ast self index in
            let type' = map_type self type' in
            Lsubscript { value; index; span; type' })
    | Tassign { name = name, type'; value; span } -> (
        match Type_env.find self name with
        | None -> failwith "unbound var"
        | Some _ ->
            let value = map_ast self value in
            let type' = map_type self type' in
            Lassign { name = (name, type'); value; span })
    | Tassign_subscript { name; index; value; span } -> (
        match Type_env.find self name with
        | None -> failwith "unbound var"
        | Some _ ->
            let value = map_ast self value in
            let index = map_ast self index in

            Lassign_subscript { name; index; value; span })
    | Tupdate_ref { name; value; span } -> (
        match Type_env.find self name with
        | None -> failwith "unbound var"
        | Some _ ->
            let value = map_ast self value in
            Lupdate_ref { name; value; span })
    | Tderef { name; span; type' } -> (
        match Type_env.find self name with
        | None -> failwith "unbound var"
        | Some _ ->
            let type' = map_type self type' in
            Lderef { name; span; type' })
    | Tcase { case; value; span } ->
        Lcase { case; value = map_ast self value; span }
    | Trecord { proto = proto, proto_type; fields; span; type' } ->
        let proto_type = map_type self proto_type in
        let type' = map_type self type' in
        let fields =
          List.map fields ~f:(fun (name, t) ->
              let t = map_ast self t in
              (name, t))
        in
        Lrecord { proto = (proto, proto_type); fields; span; type' }
    | Tmatch { value; cases; type'; span } ->
        let value = map_ast self value in
        let type' = map_type self type' in
        let cases =
          List.map cases ~f:(fun (tag, (name, type'), expr) ->
              let type' = map_type self type' in
              let expr =
                map_ast (Type_env.add self ~key:name ~data:type') expr
              in
              (tag, (name, type'), expr))
        in
        Lmatch { value; cases; type'; span }
    | Tselect { value; field; span; type' } ->
        let value = map_ast self value in
        let type' = map_type self type' in
        Lselect { value; field; span; type' }
    | Tlambda { closure } ->
        let (Tclosure { span; _ }) = closure in
        let closure = map_closure ~is_rec:None self closure in
        let name = Fresh_sym.f () in
        let type' = Closure.type_of closure in
        let app = Lvar { value = name; span; type' } in
        Ldef { name; closure; span; app; fn_type = type' }
    | Tvar { value; span; type' } -> (
        match Type_env.find self value with
        | None -> failwith "unbound var"
        | Some _ ->
            let type' = map_type self type' in
            Lvar { value; span; type' })
    | Tlet { binding = name, type'; value; app; span } ->
        let value = map_ast self value in
        let type' = map_type self type' in
        let self = Type_env.add self ~key:name ~data:type' in
        let app = map_ast self app in
        Llet { binding = (name, type'); value; app; span }
    | Tseq { first; second; span } ->
        let first = map_ast self first in
        let second = map_ast self second in
        Lseq { first; second; span }
    | Tapp { fn; value; type'; span } ->
        let fn = map_ast self fn in
        let value = map_ast self value in
        let type' = map_type self type' in
        Lapp { fn; value; type'; span }
    | Tprimop { op; args; type'; span } ->
        let args = List.map args ~f:(map_ast self) in
        let type' = map_type self type' in
        let op = map_primop self op in
        Lprimop { op; args; type'; span }
    | Tdef { name; closure; fn_type; app; span } ->
        let fn_type = map_type self fn_type in
        let self = Type_env.add ~key:name ~data:fn_type self in
        let closure = map_closure ~is_rec:(Some name) self closure in
        let app = map_ast self app in
        Ldef { name; closure; span; app; fn_type }
    | Tif { cond; then_; else_; span; type' } ->
        let cond = map_ast self cond in
        let then_ = map_ast self then_ in
        let else_ = map_ast self else_ in
        let type' = map_type self type' in
        Lif { cond; then_; else_; span; type' }
    | Tfor { iterate; body; span; type' } ->
        let self, iterate = map_iterate self iterate in
        let body = map_ast self body in
        let type' = map_type self type' in
        Lfor { iterate; body; span; type' }

  and map_iterate self iterate =
    let rec loop self iter =
      let open Tast.Iterate in
      let open Lambda.Iterate in
      match iter with
      | Tdone -> (self, Ldone)
      | Titerate { name = name, type'; start; finish; is_ascending; span; rest }
        ->
          let start = map_ast self start in
          let finish = map_ast self finish in
          let type' = map_type self type' in
          let self = Type_env.add self ~key:name ~data:type' in
          let self, rest = loop self rest in
          (self, Literate { name; start; finish; is_ascending; span; rest })
    in
    loop self iterate

  and map_closure ~is_rec self
      (Tclosure { parameter = value, type'; value = closure_body; span }) =
    match is_rec with
    | None ->
        let type' = map_type self type' in
        let self = Type_env.add self ~key:value ~data:type' in
        let closure_body = map_ast self closure_body in
        let parameter = (value, type') in
        Lclosure { parameter; value = closure_body; span }
    | Some fix ->
        let type' = map_type self type' in
        let self = Type_env.add self ~key:value ~data:type' in
        let closure_body = map_ast self closure_body in
        let parameter = (value, type') in
        Lrec_closure { self = fix; parameter; value = closure_body; span }

  and map_primop _self =
    let open Tast.Primop in
    let open Lambda.Primop in
    function
    | Tint_add -> Lint_add
    | Tint_sub -> Lint_sub
    | Tint_mul -> Lint_mul
    | Tint_div -> Lint_div
    | Tint_eq -> Lint_eq
    | Tint_lt -> Lint_lt
    | Tint_gt -> Lint_gt
    | Tint_le -> Lint_le
    | Tint_ge -> Lint_ge
    | Tint_ne -> Lint_neq
    | Tint_neg -> Lint_neg
    | Tfloat_add -> Lfloat_add
    | Tfloat_sub -> Lfloat_sub
    | Tfloat_mul -> Lfloat_mul
    | Tfloat_div -> Lfloat_div
    | Tfloat_eq -> Lfloat_eq
    | Tfloat_lt -> Lfloat_lt
    | Tfloat_gt -> Lfloat_gt
    | Tfloat_le -> Lfloat_le
    | Tfloat_ge -> Lfloat_ge
    | Tfloat_ne -> Lfloat_neq
    | Tfloat_neg -> Lfloat_neg
    | Tbool_eq -> Lbool_eq
    | Tbool_ne -> Lbool_neq
    | Tstr_concat -> Lstr_concat
    | Tvector_concat -> Lvector_concat

  and map_type _self t =
    let open Tast in
    let recursive = ref (Map.empty (module Polar.Variable)) in
    let rec f polar
        (in_process :
          (Polar.Variable.t, Polar.Variable.comparator_witness) Set.t) =
      let open Polar.Type in
      let open Simple_type in
      let open Lambda.Type in
      let open Core in
      match polar with
      | PolarType { type' = Sint_type; _ } -> Ty_int
      | PolarType { type' = Sbool_type; _ } -> Ty_bool
      | PolarType { type' = Sfloat_type; _ } -> Ty_float
      | PolarType { type' = Sunit_type; _ } -> Ty_unit
      | PolarType { type' = Schar_type; _ } -> Ty_char
      | PolarType { type' = Sfunction_type { argument; result }; polar } ->
          let argument = Polar.Type.create argument (Polar.not polar) in
          let result = Polar.Type.create result polar in
          Ty_function
            { argument = f argument in_process; result = f result in_process }
      | PolarType { type' = Smutable { read; write; _ }; polar } ->
          let read =
            let read = Polar.Type.create read polar in
            f read in_process
          in
          let write =
            let write = Polar.Type.create write polar in
            f write in_process
          in
          Ty_mutable { read; write }
      | PolarType { type' = Ssparse_tuple _; _ } ->
          failwith "sparse tuple not supported yet"
      | PolarType { type' = Stuple_type { first; second; rest }; polar } ->
          let first = Polar.Type.create first polar in
          let second = Polar.Type.create second polar in
          let rest = List.map rest ~f:(fun t -> Polar.Type.create t polar) in
          let first = f first in_process in
          let second = f second in_process in
          let rest = List.map rest ~f:(fun t -> f t in_process) in
          Ty_tuple { first; second; rest }
      | PolarType { type' = Sstring_type; _ } -> Ty_string
      | PolarType
          {
            type' = Svector_type { read = Some read; write = Some write; _ };
            polar;
          } ->
          let read = Polar.Type.create read polar in
          let write = Polar.Type.create write (Polar.not polar) in
          let read = f read in_process in
          let write = f write in_process in
          Ty_vector { read; write }
      | PolarType { type' = Svector_type { read = None; write = None; _ }; _ }
        ->
          Ty_vector { read = Ty_void; write = Ty_void }
      | PolarType
          { type' = Svector_type { read = Some read; write = None; _ }; polar }
        ->
          Ty_vector
            {
              read = f (Polar.Type.create read polar) in_process;
              write = Ty_void;
            }
      | PolarType
          { type' = Svector_type { read = None; write = Some write; _ }; polar }
        ->
          Ty_vector
            {
              write = f (Polar.Type.create write (Polar.not polar)) in_process;
              read = Ty_void;
            }
      | PolarType
          {
            type' = Sreference { read = Some read; write = Some write; _ };
            polar;
          } ->
          let read = Polar.Type.create read polar in
          let write = Polar.Type.create write (Polar.not polar) in
          let read = f read in_process in
          let write = f write in_process in
          Ty_reference { read; write }
      | PolarType { type' = Sreference { read = None; write = None; _ }; _ } ->
          failwith "vector type not supported yet"
      | PolarType
          { type' = Sreference { read = Some read; write = None; _ }; polar } ->
          Ty_reference
            {
              read = f (Polar.Type.create read polar) in_process;
              write = Ty_void;
            }
      | PolarType
          { type' = Sreference { read = None; write = Some write; _ }; polar }
        ->
          Ty_reference
            {
              write = f (Polar.Type.create write (Polar.not polar)) in_process;
              read = Ty_void;
            }
      | PolarType { type' = Srecord { fields }; polar } ->
          let fields =
            List.map fields ~f:(fun (name, t) ->
                let t = Polar.Type.create t polar in
                (name, f t in_process))
          in
          Ty_record { fields }
      | PolarType { type' = Scases { cases }; polar } ->
          let cases =
            List.map cases ~f:(fun (name, t) ->
                let t = Polar.Type.create t polar in
                (name, f t in_process))
          in
          Ty_cases { cases }
      | PolarType { type' = Svar_type { state }; polar } -> (
          let polar_var = Polar.Variable.create state polar in
          match
            Set.find in_process ~f:(fun p ->
                let open Polar.Variable in
                polar_var = p)
          with
          | Some t ->
              let name =
                match Map.find !recursive t with
                | Some name -> name
                | None ->
                    let name = Fresh_sym.f () in
                    recursive := Map.add_exn !recursive ~key:t ~data:name;
                    name
              in
              Ty_variable { name }
          | None ->
              let bounds =
                if Polar.bool polar then state.lower_bounds
                else state.upper_bounds
              in
              let bound_types =
                List.map bounds ~f:(fun t ->
                    f (Polar.Type.create t polar) (Set.add in_process polar_var))
              in
              let merger lhs rhs =
                if Polar.bool polar then Ty_union { lhs; rhs }
                else Ty_intersection { lhs; rhs }
              in
              let res =
                List.fold_left bound_types
                  ~init:(Ty_variable { name = Variable.name state })
                  ~f:merger
              in
              let found = Map.find !recursive polar_var in
              Option.fold ~init:res
                ~f:(fun _ name -> Ty_recursive { body = res; name })
                found)
    in
    f (Polar.Type.create t Polar.Positive) (Set.empty (module Polar.Variable))
end

module Tests = struct
  let run_it s =
    match
      (let open Or_error.Let_syntax in
       let%bind ast = To_syntax.parse s in
       let (module Fresh_sym) = Text.create_fresh_sym () in
       let (module Fresh_var) =
         Typing.create_fresh_vars ~fresher:(module Fresh_sym)
       in
       let (module S) =
         (module struct
           module Fresh_sym = Fresh_sym
           module Fresh_var = Fresh_var
         end : Typing.S)
       in
       let (module Type_term) = (module Typing.Make (S) : Typing.MAPPER) in
       let%bind t = Type_term.map ast in
       let (module To_lambda) = (module Make (Fresh_sym) : MAPPER) in
       let%bind t = To_lambda.map t in
       let open Lambda in
       t |> Lambda.type_of |> Type.to_string |> Ok)
      |> Or_error.ok_exn |> print_endline
    with
    | () -> ()
    | exception exn -> print_endline (Exn.to_string exn)

  let%expect_test "type empty file" =
    run_it {| |};
    [%expect {| () |}]

  let%expect_test "type int" =
    run_it {| 1 |};
    [%expect {| int  |}]

  let%expect_test "type neg int" =
    run_it {| - 1 |};
    [%expect {| int  |}]

  let%expect_test "parse float" =
    run_it "3.14";
    [%expect {| float |}]

  let%expect_test "vars must be defined" =
    run_it {| x |};
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "type rec" =
    run_it {| { a=0, b=1} |};
    [%expect {| {a: int, b: int} |}]

  let%expect_test "field access" =
    run_it {| { a=0, b=1}.a |};
    [%expect {| '__2 | int |}]

  let%expect_test "type lambda" =
    run_it {| fn x -> 10 end |};
    [%expect {| '__0 -> int |}]

  let%expect_test "type let" =
    run_it {| let x = 0 in x|};
    [%expect {| int  |}]

  let%expect_test "type let function" =
    run_it {| let f x -> 0 in f |};
    [%expect {| '__1 -> int |}]

  let%expect_test "type app" =
    run_it {| let f x -> 0 in f 2 |};
    [%expect {|
      '__1 | int|}]

  let%expect_test "type def function" =
    run_it {|
      def f x = f 0 in 
      f
    |};
    [%expect {| '__3 | '__5 -> '__4 |}]

  let%expect_test "type binop" =
    run_it "1 + 2";
    [%expect {| int  |}]

  let%expect_test "type binop precedence" =
    run_it "1 + 2 * 3";
    [%expect {| int  |}]

  let%expect_test "type relop" =
    run_it "1 < 2";
    [%expect {| bool |}]

  let%expect_test "type binop paren" =
    run_it "(1 + 2) * 3";
    [%expect {| int  |}]

  let%expect_test "if end" =
    run_it "if 1 < 2 then 4 end";
    [%expect {| '__0 | () | int |}]

  let%expect_test "if" =
    run_it "if true then 1 else 0";
    [%expect {| '__0 | int | int |}]

  let%expect_test "if union" =
    run_it "if true then 1 else 0.0";
    [%expect {| '__0 | float | int |}]

  let%expect_test "paren" =
    run_it "(1)";
    [%expect {| int  |}]

  let%expect_test "tuple" =
    run_it "(1, 2)";
    [%expect {| (int, int) |}]

  let%expect_test "tuple single" =
    run_it "(1,)";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

  let%expect_test "tuple index" =
    run_it "(1, 2).0";
    [%expect {| '__0 | int |}]

  let%expect_test "triple index" =
    run_it "(1, 2, true).2";
    [%expect {| '__0 | bool |}]

  let%expect_test "empty vector" =
    run_it "[||]";
    [%expect {| vector['__0, void] |}]

  let%expect_test "vector" =
    run_it "[| 1, 2 |]";
    [%expect {| vector['__0 | int | int, void] |}]

  let%expect_test "union vector" =
    run_it "[| 1, true |]";
    [%expect {| vector['__0 | bool | int, void] |}]

  let%expect_test "vector subscript" =
    run_it {| let xs = [| 1, 2 |] in xs[0] |};
    [%expect {| '__1 | int |}]

  let%expect_test "union vector subscript" =
    run_it {| let xs = [| 1, true |] in xs[0] |};
    [%expect {| '__1 | int | bool |}]

  let%expect_test "local readwrite vector" =
    run_it {|
       let xs = mut [| 0, 1|] in
       xs[0] = 1 |};
    [%expect {| () |}]

  let%expect_test "readwrite vector, capture readonly" =
    run_it
      {|
       let xs = mut [| 0, 1|] in
       let f x -> xs[0] = x in
       xs[0] = 1;
       f 1|};
    [%expect {| ("Fx__Typing.Type_error(_, _, _)") |}]

  let%expect_test "readwrite vector, capture readwrite" =
    run_it
      {|
       let xs = ref [| 0, 1|] in
       let f x -> xs[0] = x in
       xs[0] = 1;
       f 1|};
    [%expect {| '__3 | () |}]

  let%expect_test "reference vectors can be readonly" =
    run_it {|
      let xs = ref [| 0, 1|] in
      xs[..] |};
    [%expect {| vector['__1 | int | int, void] |}]

  let%expect_test "reference vectors can be writeonly" =
    run_it {|
      let xs = ref [| 0, 1|] in
      &mut xs[..] |};
    [%expect {| vector['__1 | int | int, '__0] |}]

  let%expect_test "readonly vectors can be read" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = xs[..] in
      ys[1] |};
    [%expect {| '__2 | int |}]

  let%expect_test "readwrite slices can be written" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = &mut xs[..] in
      ys[1] = 10 |};
    [%expect {| () |}]

  let%expect_test "let ref" =
    run_it "let ref a = 0 in a";
    [%expect {| ref['__0 | int, void] |}]

  let%expect_test "let ref update" =
    run_it "let ref a = 0 in a := 1";
    [%expect {| () |}]

  let%expect_test "let ref deref" =
    run_it "let ref a = 0 in !a";
    [%expect {| '__1 | int |}]

  let%expect_test "let ref update union" =
    run_it {|
      let ref a = 0 in
      a := 1.0;
      a
    |};
    [%expect {| ref['__0 | float | int, void] |}]

  let%expect_test "let ref deref union" =
    run_it {|
      let ref a = 0 in
      a := 1.0;
      !a
    |};
    [%expect {| '__1 | int | float |}]

  let%expect_test "let mut" =
    run_it "let mut x = 1 in x";
    [%expect {| '__0 | int |}]

  let%expect_test "seq" =
    run_it "1; 2";
    [%expect {| int  |}]

  let%expect_test "let mut update" =
    run_it "let mut a = 0 in a = 1";
    [%expect {| () |}]

  let%expect_test "declare mut closure" =
    run_it {|
      let mut f = fn x -> x end in
      f 0
    |};
    [%expect {| '__3 | int |}]

  let%expect_test "mut closure" =
    run_it
      {|
      let mut f = fn x -> x end in
      f 0;
      (f = (fn x -> x + 1 end));
      f 0
    |};
    [%expect {| '__3 | int |}]

  let%expect_test "forever" =
    run_it {| for do () end |};
    [%expect {| () |}]

  let%expect_test "ref counter loop" =
    run_it
      {| 
      let ref i = 0 in
      for do
        i := (!i + 1)
      end;
      !i
    |};
    [%expect {| '__2 | int |}]

  let%expect_test "for range" =
    run_it
      {| 
        let f x -> () in
        for i <- 0 to 10 do
          f i
        end
        |};
    [%expect {| () |}]

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
    [%expect {| () |}]

  let%expect_test "for decreasing loop" =
    run_it
      {| 
        for 
          i <- 0 downto 10
        do
          ()
        end
        |};
    [%expect {| () |}]

  let%expect_test "a char" =
    run_it {| 'a' |};
    [%expect {| char |}]

  let%expect_test "strings" =
    run_it {| "hello world" |};
    [%expect {| string |}]

  let%expect_test "string concat" =
    run_it {| "hello" ^ "world" |};
    [%expect {| string |}]

  let%expect_test "record subtype" =
    run_it
      {|
        if true then
          { a = 0, b = 1 }
        else
          { a = 0, b = 1, c = 2 }
    |};
    [%expect {| '__0 | {a: int, b: int, c: int} | {a: int, b: int} |}]

  let%expect_test "record contraction" =
    run_it
      {|
        let r = if true then
          { a = 0, b = 1 }
        else
          { a = 0, b = 1, c = 2 } in
        r.c
    |};
    [%expect {| ("Fx__Typing.Missing_record_field(_)") |}]

  let%expect_test "record union" =
    run_it
      {|
       if true then
          { a = 0, b = 1 }
        else
          { a = 0, b = '1' } 
    |};
    [%expect {| '__0 | {a: int, b: char} | {a: int, b: int} |}]

  let%expect_test "record union fields" =
    run_it
      {|
        let r = if true then
          { a = 0, b = 1 }
        else
          { a = 0, b = '1' } in
        r.b
    |};
    [%expect {| '__5 | int | char |}]

  let%expect_test "record fun" =
    run_it {|
      let f r -> r.a in
      f { a = 0, b = 1 }
   |};
    [%expect {| '__2 | int |}]

  let%expect_test "record fun union" =
    run_it
      {|
      let f r -> r.a in
      f { a = 0, b = 1 } + f { a = 0, b = '1' }
   |};
    [%expect {| ("Fx__Typing.Constrain_error(_, 0)") |}]

  let%expect_test "cons list" =
    run_it
      {|
      def produce arg = { head = arg, tail = (produce (arg + 1)) } in 
      produce
    |};
    [%expect
      {| '__5 | '__6 & int -> {head: '__6 | int, tail: '__7 | {head: '__6 | int, tail: '__15} as __15} |}]

  let%expect_test "cons list as record" =
    run_it
      {|
      def consume strm = (strm.head + consume strm.tail) in
      consume
    |};
    [%expect
      {| '__5 | '__6 & {tail: '__8 & '__17} & {head: '__7 & int} as __17 -> int |}]

  let%expect_test "cons list as record" =
    run_it
      {|
      def produce arg = { head = arg, tail = (produce (arg + 1)) } in 
      def consume strm = (strm.head + consume strm.tail) in
    
      let codata = produce 42 in
      consume codata
    |};
    [%expect {| '__14 | int |}]

  let%expect_test "nested function" =
    run_it
      {|
      let f = 
        let x = 0 in
        fn y -> x + y end in
      f
    |};
    [%expect {| '__1 & int -> int |}]

  let%expect_test "curried function" =
    run_it {|
      let f x -> fn y -> x + y end in
      f
    |};
    [%expect {| '__3 & int -> '__2 & int -> int |}]

  let%expect_test "iife" =
    run_it {| (fn x -> x end) 0   |};
    [%expect {| '__0 | int |}]

  let%expect_test "cases" =
    run_it {| Some 1 |};
    [%expect {| [case Some int] |}]

  let%expect_test "unit case" =
    run_it {| None |};
    [%expect {| [case None ()] |}]

  let%expect_test "disjoint match" =
    run_it
      {| 
        match B
          case A x -> x.foo,
          case B -> false
        end
    |};
    [%expect {|  '__0 | bool |}]
end
