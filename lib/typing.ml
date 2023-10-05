open Core
open Text
open Tast

exception Unbound_variable of { value : Symbol.t; span : Span.t }
exception Missing_record_field of { value : Symbol.t }
exception Constrain_error of { lhs : SimpleType.t; rhs : SimpleType.t }

module type FRESH_VAR = sig
  val f : Level.t -> SimpleType.t
end

module type SCHEME = sig
  type t = private Sch_simple of SimpleType.t | Sch_poly of PolymorhicType.t

  val of_simple_type : SimpleType.t -> t
  val of_polymorphic_type : PolymorhicType.t -> t
  val instantiate : t -> Level.t -> SimpleType.t
  val level : t -> Level.t
end

module rec Scheme : functor (Fresh_var : FRESH_VAR) -> SCHEME =
functor
  (Fresh_var : FRESH_VAR)
  ->
  struct
    type t = Sch_simple of SimpleType.t | Sch_poly of PolymorhicType.t

    let of_simple_type t = Sch_simple t
    let of_polymorphic_type t = Sch_poly t

    let instantiate t lvl =
      let open SimpleType in
      let open PolymorhicType in
      match t with
      | Sch_simple t -> t
      | Sch_poly (PolymorhicType { level = limit; body }) ->
          let freshen_above limit ty lvl =
            let freshened =
              object
                val mutable freshened = Map.empty (module Variable)
                method get v = Map.find freshened v

                method set v t =
                  freshened <- Map.add_exn freshened ~key:v ~data:t
              end
            in

            let rec freshen ty =
              let type_level = SimpleType.level ty in
              if Level.compare type_level limit < 1 then ty
              else
                match ty with
                | Svar_type { state } -> (
                    match freshened#get state with
                    | Some state -> Svar_type { state }
                    | None ->
                        let v =
                          match Fresh_var.f lvl with
                          | Svar_type { state } -> state
                          | _ -> assert false
                        in
                        freshened#set state v;
                        let lower_bounds = Variable.lower_bounds state in
                        let upper_bounds = Variable.upper_bounds state in
                        lower_bounds :=
                          List.rev
                            (List.map (List.rev !lower_bounds) ~f:(fun t ->
                                 freshen t));
                        upper_bounds :=
                          List.rev
                            (List.map (List.rev !upper_bounds) ~f:(fun t ->
                                 freshen t));
                        Variable.to_simple_type v)
                | Sfunction_type { argument; result } ->
                    Sfunction_type
                      { argument = freshen argument; result = freshen result }
                | Srecord { fields } ->
                    Srecord
                      {
                        fields =
                          List.map fields ~f:(fun (f, t) -> (f, freshen t));
                      }
                | Sint_type -> Sint_type
            in
            freshen ty
          in
          freshen_above limit body lvl

    and level = function
      | Sch_simple t -> SimpleType.level t
      | Sch_poly (PolymorhicType { level; _ }) -> level
  end

module Polar = struct
  type polarity = Use | Value [@@deriving sexp, compare]

  let not = function Use -> Value | Value -> Use
  let bool = function Use -> true | Value -> false
  let of_bool = function true -> Use | false -> Value

  module Type = struct
    module T = struct
      type t = PolarType of { type' : SimpleType.t; polar : polarity }
      [@@deriving sexp, compare]

      let create type' polar = PolarType { type'; polar }
    end

    include T
    include Comparable.Make (T)

    let level (PolarType { type'; _ }) = SimpleType.level type'

    let not (PolarType { type'; polar }) =
      PolarType { type'; polar = not polar }

    let bool (PolarType { polar; _ }) = bool polar
    let polarity (PolarType { polar; _ }) = polar
    let type_of (PolarType { type'; _ }) = type'
  end

  module Variable = struct
    module T = struct
      type t = PolarVariable of { state : Variable.t; polar : polarity }
      [@@deriving sexp, compare]

      let create state polar = PolarVariable { state; polar }
    end

    include T
    include Comparable.Make (T)

    let level (PolarVariable { state; _ }) = Variable.level state

    let not (PolarVariable { state; polar }) =
      PolarVariable { state; polar = not polar }

    let bool (PolarVariable { polar; _ }) = bool polar
    let polarity (PolarVariable { polar; _ }) = polar
  end
end

module Extrude = struct
  module type T = sig
    val f : Polar.Type.t -> SimpleType.t
  end

  module Self = struct
    type t = Self of { level : Level.t }

    let create level = Self { level }
    let level (Self { level; _ }) = level
  end

  module Make (S : sig
    val level : Level.t
  end) : T = struct
    let c =
      object
        val mutable map = Map.empty (module Polar.Variable)
        method add ~key ~data = map <- Map.add_exn map ~key ~data

        method find_or_else key ~else' =
          match Map.find map key with Some v -> v | None -> else' ()
      end

    let rec extrude self t =
      let open Level in
      if Polar.Type.level t <= Self.level self then Polar.Type.type_of t
      else
        let open SimpleType in
        let polarity = Polar.Type.polarity t in
        let create_type t = Polar.Type.create t polarity in
        match Polar.Type.type_of t with
        | Sint_type -> Sint_type
        | Sfunction_type { argument; result } ->
            Sfunction_type
              {
                argument =
                  extrude self (Polar.Type.create argument (Polar.not polarity));
                result = extrude self (create_type result);
              }
        | Srecord { fields } ->
            Srecord
              {
                fields =
                  List.map fields ~f:(fun (f, t) ->
                      (f, extrude self (create_type t)));
              }
        | Svar_type { state } ->
            let polar = Polar.Variable.create state polarity in
            c#find_or_else polar ~else':(fun () ->
                let nvs = Variable.create (Self.level self) in
                let nvs_state =
                  nvs |> Variable.of_simple_type |> Option.value_exn
                in
                c#add ~key:polar ~data:(Svar_type { state = nvs_state });
                (if Polar.bool polarity then (
                   let upper_bounds = Variable.upper_bounds state in
                   upper_bounds :=
                     Svar_type { state = nvs_state } :: !upper_bounds;
                   let lower_bounds = Variable.lower_bounds nvs_state in
                   Variable.lower_bounds nvs_state
                   := List.map !lower_bounds ~f:(fun ty ->
                          extrude self (Polar.Type.create ty polarity)))
                 else
                   let lower_bounds = Variable.lower_bounds state in
                   lower_bounds :=
                     Svar_type { state = nvs_state } :: !lower_bounds;
                   let upper_bounds = Variable.upper_bounds nvs_state in
                   Variable.upper_bounds nvs_state
                   := List.map !upper_bounds ~f:(fun ty ->
                          extrude self (Polar.Type.create ty polarity)));
                nvs)

    let f t = extrude (Self.create S.level) t
  end
end

module Constrain = struct
  module type T = sig
    val f : SimpleType.t -> SimpleType.t -> unit
  end

  module Make () : T = struct
    module Constraint = struct
      module T = struct
        type t = Constraint of { lhs : SimpleType.t; rhs : SimpleType.t }
        [@@deriving sexp, compare]
      end

      include T
      include Comparable.Make (T)
    end

    let cache =
      object
        val mutable cache = Set.empty (module Constraint)
        method contains c = Set.mem cache c
        method add c = cache <- Set.add cache c
      end

    let extrude type' polarity level =
      let module E = Extrude.Make (struct
        let level = level
      end) in
      let polar_type = Polar.Type.create type' polarity in
      E.f polar_type

    let rec f lhs rhs =
      let open Constraint in
      constrain (Constraint { lhs; rhs })

    and constrain (Constraint.Constraint { lhs; rhs } as c) =
      if not (cache#contains c) then (
        cache#add c;
        let open Level in
        match (lhs, rhs) with
        | Sint_type, Sint_type -> ()
        | ( Sfunction_type { argument = lhs_arg; result = lhs_res },
            Sfunction_type { argument = rhs_arg; result = rhs_res } ) ->
            constrain (Constraint { lhs = lhs_arg; rhs = rhs_arg });
            constrain (Constraint { lhs = lhs_res; rhs = rhs_res })
        | Srecord { fields = lhs_fields }, Srecord { fields = rhs_fields } ->
            List.iter rhs_fields ~f:(fun (rhs_field, rhs_type) ->
                match
                  List.Assoc.find lhs_fields rhs_field ~equal:Symbol.equal
                with
                | Some lhs_type ->
                    constrain (Constraint { lhs = lhs_type; rhs = rhs_type })
                | None -> raise (Missing_record_field { value = rhs_field }))
        | Svar_type { state = lhs_state }, rhs
          when SimpleType.level rhs <= Variable.level lhs_state ->
            Variable.upper_bounds lhs_state
            := rhs :: !(Variable.upper_bounds lhs_state);
            List.iter
              !(Variable.lower_bounds lhs_state)
              ~f:(fun lhs -> constrain (Constraint { lhs; rhs }))
        | lhs, Svar_type { state = rhs_state }
          when SimpleType.level lhs <= Variable.level rhs_state ->
            Variable.lower_bounds rhs_state
            := lhs :: !(Variable.lower_bounds rhs_state);
            List.iter
              !(Variable.upper_bounds rhs_state)
              ~f:(fun rhs -> constrain (Constraint { lhs; rhs }))
        | (Svar_type { state = lhs_state } as lhs), rhs ->
            let level = Variable.level lhs_state in
            let rhs = extrude rhs (Polar.of_bool false) level in
            constrain (Constraint { lhs; rhs })
        | lhs, (Svar_type { state = rhs_state } as rhs) ->
            let level = Variable.level rhs_state in
            let lhs = extrude lhs (Polar.of_bool true) level in
            constrain (Constraint { lhs; rhs })
        | lhs, rhs -> raise (Constrain_error { lhs; rhs }))
      else ()
  end
end

module type MAPPER = sig
  type self

  val create : unit -> self
  val visit : Syntax.t -> Tast.t Or_error.t
  val visit_ast : self -> Syntax.t -> Tast.t
  val visit_closure : self -> Syntax.Closure.t -> Closure.t
  val visit_pattern : self -> Syntax.Pattern.t -> Pattern.t
end

module rec Make : functor (Fresh_var : FRESH_VAR) -> MAPPER =
functor
  (Fresh_var : FRESH_VAR)
  ->
  struct
    module TypeScheme = Scheme (Fresh_var)

    module Self = struct
      type t =
        | Self of {
            self : (Symbol.t, TypeScheme.t, Symbol.comparator_witness) Map.t;
            level : Level.t;
          }

      let create () =
        Self { self = Map.empty (module Symbol); level = Level.default }

      let contains (Self { self; _ }) v = Map.mem self v
      let level (Self { level; _ }) = level

      let level_up (Self { self; level }) =
        Self { self; level = Level.level_up level }

      let find (Self { self; _ }) v = Map.find_exn self v

      let add (Self { self; level }) ~key ~data =
        Self { self = Map.add_exn self ~key ~data; level }
    end

    type self = Self.t

    let create () = Self.create ()

    let constrain lhs rhs =
      let module C = Constrain.Make () in
      C.f lhs rhs

    let rec visit_ast self =
      let open Syntax in
      let open Tast.T in
      function
      | Uint { value; span } -> Tint { value; span }
      | Uvar { value; span } when Self.contains self value ->
          let type' = Self.find self value in
          let type' = TypeScheme.instantiate type' (Self.level self) in
          Tvar { value; span; type' }
      | Uvar { value; span } -> raise (Unbound_variable { value; span })
      | Urecord { fields; span } ->
          Trecord
            {
              fields =
                List.map fields ~f:(fun (field, e) -> (field, visit_ast self e));
              span;
            }
      | Uselect { value; field; span } ->
          let res = Fresh_var.f (Self.level self) in
          let value = visit_ast self value in
          constrain (Tast.T.type_of value)
            (SimpleType.Srecord { fields = [ (field, res) ] });
          Tselect { value; field; span; type' = res }
      | Ulambda { closure } -> Tlambda { closure = visit_closure self closure }
      | Ulet
          {
            pattern = Upat_var { value = name; span = pat_span };
            value;
            app;
            span;
          } ->
          let value = visit_ast (Self.level_up self) value in
          let level = Self.level self in
          let type' = PolymorhicType.create level (Tast.T.type_of value) in
          let type' = TypeScheme.of_polymorphic_type type' in
          let app = visit_ast (Self.add self ~key:name ~data:type') app in

          Tlet
            {
              pattern =
                Tpat_var
                  {
                    value = name;
                    span = pat_span;
                    type' = TypeScheme.instantiate type' level;
                  };
              value;
              app;
              span;
            }
      | Ulet_fun { name; closure; app; span } ->
          visit_ast self
            (Ulet
               {
                 pattern = Upat_var { value = name; span };
                 value = Ulambda { closure };
                 app;
                 span;
               })
      | Udef { name; closure; app; span } ->
          let expr = Fresh_var.f (Level.level_up (Self.level self)) in
          let closure =
            let type' = TypeScheme.of_simple_type expr in
            let self = Self.add self ~key:name ~data:type' in
            let self = Self.level_up self in
            visit_closure self closure
          in
          constrain (Tast.Closure.type_of closure) expr;
          let self =
            let poly_type = PolymorhicType.create (Self.level self) expr in
            Self.add self ~key:name
              ~data:(TypeScheme.of_polymorphic_type poly_type)
          in
          let app = visit_ast self app in
          Tdef { name; closure; app; span }
      | Uapp { fn; value; span } ->
          let res = Fresh_var.f (Self.level self) in
          let fn = visit_ast self fn in
          let arg = visit_ast self value in
          constrain (Tast.T.type_of fn)
            (SimpleType.Sfunction_type
               { argument = Tast.T.type_of arg; result = res });
          Tapp { fn; value = arg; span; type' = res }

    and visit_closure self
        (Syntax.Uclosure
          {
            parameter = Upat_var { value = name; span = pat_span };
            value;
            span;
          }) =
      let type' = Fresh_var.f (Self.level self) in

      Tclosure
        {
          parameter = Tpat_var { value = name; span = pat_span; type' };
          value = visit_ast self value;
          span;
        }

    let visit_pattern _self _pattern = failwith "TODO"

    let visit ast =
      Or_error.try_with (fun () ->
          let self = create () in
          visit_ast self ast)
  end

module Tests = struct
  let run_it s =
    match
      (let open Or_error.Let_syntax in
       let%bind ast = To_syntax.parse s in
       let (module Fresh_var) =
         (module struct
           let counter = ref 0

           let f level =
             let v = !counter in
             counter := v + 1;
             Variable.create level
         end : FRESH_VAR)
       in
       let (module Visitor) = (module Make (Fresh_var) : MAPPER) in
       let%bind t = Visitor.visit ast in
       t |> Tast.T.type_of |> SimpleType.sexp_of_t |> Sexp.to_string |> Ok)
      |> Or_error.ok_exn |> print_endline
    with
    | () -> ()
    | exception exn -> print_endline (Exn.to_string exn)

  let%expect_test "type empty file" =
    run_it {| |};
    [%expect {| Sint_type |}]

  let%expect_test "type int" =
    run_it {| 1 |};
    [%expect {| Sint_type |}]

  let%expect_test "vars must be defined" =
    run_it {| x |};
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "type rec" =
    run_it {| { a=0, b=1} |};
    [%expect
      {| (Srecord(fields(((Symbol a)Sint_type)((Symbol b)Sint_type)))) |}]

  let%expect_test "field access" =
    run_it {| { a=0, b=1}.a |};
    [%expect
      {| (Svar_type(state(VariableState(level(Level(value 0)))(lower_bounds(Sint_type))(upper_bounds())))) |}]

  let%expect_test "type lambda" =
    run_it {| fn x -> 10 |};
    [%expect
      {| (Sfunction_type(argument(Svar_type(state(VariableState(level(Level(value 0)))(lower_bounds())(upper_bounds())))))(result Sint_type)) |}]

  let%expect_test "type let" =
    run_it {| let x = 0 in x|};
    [%expect {| Sint_type |}]

  let%expect_test "type let function" =
    run_it {| let f x -> 0 in f |};
    [%expect
      {| (Sfunction_type(argument(Svar_type(state(VariableState(level(Level(value 0)))(lower_bounds())(upper_bounds())))))(result Sint_type)) |}]

  let%expect_test "type app" =
    run_it {| let f x -> 0 in f 2 |};
    [%expect
      {| (Svar_type(state(VariableState(level(Level(value 0)))(lower_bounds(Sint_type))(upper_bounds())))) |}]

  let%expect_test "type def function" =
    run_it {|
      def f x = f 0 in 
      f
    |};
    [%expect
      {| (Svar_type(state(VariableState(level(Level(value 0)))(lower_bounds())(upper_bounds())))) |}]
end
