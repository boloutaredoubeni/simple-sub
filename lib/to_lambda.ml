open Core
open Text

module type MAPPER = sig
  type self

  val create : unit -> self
  val map : Tast.t -> Lambda.t Or_error.t
  val map_ast : self -> Tast.t -> Lambda.t

  val map_closure :
    is_rec:Symbol.t option -> self -> Tast.Closure.t -> Lambda.Closure.t

  val map_type : self -> Tast.SimpleType.t -> Lambda.Type.t
  val map_primop : self -> Tast.Primop.t -> Lambda.Primop.t
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
    | Tunit { span } -> Lunit { span }
    | Ttuple { first; second; rest; span } ->
        let first = map_ast self first in
        let second = map_ast self second in
        let rest = List.map rest ~f:(map_ast self) in
        Ltuple { first; second; rest; span }
    | Tsubscript { value; index; span; type' } ->
        let value = map_ast self value in
        let index = map_ast self index in
        let type' = map_type self type' in
        Lsubscript { value; index; span; type' }
    | Trecord { fields; span } ->
        let fields =
          List.map fields ~f:(fun (name, t) ->
              let t = map_ast self t in
              (name, t))
        in
        Lrecord { fields; span }
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
    | Tvar { value; span; _ } -> (
        match Type_env.find self value with
        | None -> failwith "unbound var"
        | Some type' -> Lvar { value; span; type' })
    | Tlet
        {
          pattern = Tpat_var { value = name; span = bind_span; _ };
          value;
          app;
          span;
        } ->
        let value = map_ast self value in
        let type' = Lambda.type_of value in
        let self = Type_env.add self ~key:name ~data:type' in
        let app = map_ast self app in
        Llet
          {
            pattern = Lparam { value = name; span = bind_span; type' };
            value;
            app;
            span;
          }
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

  and map_closure ~is_rec self
      (Tclosure
        {
          parameter = Tpat_var { value; span = bind_span; type' };
          value = closure_body;
          span;
        }) =
    match is_rec with
    | None ->
        let open Lambda.Bind in
        let type' = map_type self type' in
        let self = Type_env.add self ~key:value ~data:type' in
        let closure_body = map_ast self closure_body in
        let parameter = Lparam { value; span = bind_span; type' } in
        Lclosure { parameter; value = closure_body; span }
    | Some fix ->
        let open Lambda.Bind in
        let type' = map_type self type' in
        let self = Type_env.add self ~key:value ~data:type' in
        let closure_body = map_ast self closure_body in
        let parameter = Lparam { value; span = bind_span; type' } in
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

  and map_type _self t =
    let open Tast in
    let recursive = ref (Map.empty (module Polar.Variable)) in
    let rec f polar
        (in_process :
          (Polar.Variable.t, Polar.Variable.comparator_witness) Set.t) =
      let open Polar.Type in
      let open SimpleType in
      let open Lambda.Type in
      let open Core in
      match polar with
      | PolarType { type' = Sint_type; _ } -> Ty_int
      | PolarType { type' = Sbool_type; _ } -> Ty_bool
      | PolarType { type' = Sfloat_type; _ } -> Ty_float
      | PolarType { type' = Sunit_type; _ } -> Ty_unit
      | PolarType { type' = Sfunction_type { argument; result }; polar } ->
          let argument = Polar.Type.create argument (Polar.not polar) in
          let result = Polar.Type.create result polar in
          Ty_function
            { argument = f argument in_process; result = f result in_process }
      | PolarType { type' = Stuple_type { first; second; rest }; polar } ->
          let first = Polar.Type.create first polar in
          let second = Polar.Type.create second polar in
          let rest = List.map rest ~f:(fun t -> Polar.Type.create t polar) in
          let first = f first in_process in
          let second = f second in_process in
          let rest = List.map rest ~f:(fun t -> f t in_process) in
          Ty_tuple { first; second; rest }
      | PolarType { type' = Srecord { fields }; polar } ->
          let fields =
            List.map fields ~f:(fun (name, t) ->
                let t = Polar.Type.create t polar in
                (name, f t in_process))
          in
          Ty_record { fields }
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
                if Polar.bool polar then Variable.lower_bounds state
                else Variable.upper_bounds state
              in
              let bound_types =
                List.map !bounds ~f:(fun t ->
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
       t |> Lambda.type_of |> Type.sexp_of_t |> Sexp.to_string |> Ok)
      |> Or_error.ok_exn |> print_endline
    with
    | () -> ()
    | exception exn -> print_endline (Exn.to_string exn)

  let%expect_test "type empty file" =
    run_it {| |};
    [%expect {| Ty_int |}]

  let%expect_test "type int" =
    run_it {| 1 |};
    [%expect {| Ty_int |}]

  let%expect_test "type neg int" =
    run_it {| - 1 |};
    [%expect {| Ty_int |}]

  let%expect_test "parse float" =
    run_it "3.14";
    [%expect {| Ty_float |}]

  let%expect_test "vars must be defined" =
    run_it {| x |};
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "type rec" =
    run_it {| { a=0, b=1} |};
    [%expect {| (Ty_record(fields(((Symbol a)Ty_int)((Symbol b)Ty_int)))) |}]

  let%expect_test "field access" =
    run_it {| { a=0, b=1}.a |};
    [%expect {| (Ty_union(lhs(Ty_variable(name(Symbol __0))))(rhs Ty_int)) |}]

  let%expect_test "type lambda" =
    run_it {| fn x -> 10 |};
    [%expect
      {| (Ty_function(argument(Ty_variable(name(Symbol __0))))(result Ty_int)) |}]

  let%expect_test "type let" =
    run_it {| let x = 0 in x|};
    [%expect {| Ty_int |}]

  let%expect_test "type let function" =
    run_it {| let f x -> 0 in f |};
    [%expect
      {|
        (Ty_function(argument(Ty_variable(name(Symbol __0))))(result Ty_int)) |}]

  let%expect_test "type app" =
    run_it {| let f x -> 0 in f 2 |};
    [%expect
      {|
      (Ty_union(lhs(Ty_variable(name(Symbol __1))))(rhs Ty_int))|}]

  let%expect_test "type def function" =
    run_it {|
      def f x = f 0 in 
      f
    |};
    [%expect {| (Ty_variable(name(Symbol __6))) |}]

  let%expect_test "type binop" =
    run_it "1 + 2";
    [%expect {| Ty_int |}]

  let%expect_test "type binop precedence" =
    run_it "1 + 2 * 3";
    [%expect {| Ty_int |}]

  let%expect_test "type relop" =
    run_it "1 < 2";
    [%expect {| Ty_bool |}]

  let%expect_test "type binop paren" =
    run_it "(1 + 2) * 3";
    [%expect {| Ty_int |}]

  let%expect_test "if" =
    run_it "if true then 1 else 0";
    [%expect
      {| (Ty_union(lhs(Ty_union(lhs(Ty_variable(name(Symbol __0))))(rhs Ty_int)))(rhs Ty_int)) |}]

  let%expect_test "if union" =
    run_it "if true then 1 else 0.0";
    [%expect
      {| (Ty_union(lhs(Ty_union(lhs(Ty_variable(name(Symbol __0))))(rhs Ty_float)))(rhs Ty_int)) |}]

  let%expect_test "paren" =
    run_it "(1)";
    [%expect {| Ty_int |}]

  let%expect_test "tuple" =
    run_it "(1, 2)";
    [%expect {| (Ty_tuple(first Ty_int)(second Ty_int)(rest())) |}]

  let%expect_test "tuple single" =
    run_it "(1,)";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

  let%expect_test "tuple index" =
    run_it "(1, 2)[0]";
    [%expect {| (Ty_union(lhs(Ty_variable(name(Symbol __0))))(rhs Ty_int)) |}]
end
