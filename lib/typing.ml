open Core
open Text
open Tast

exception Unbound_variable of { value : Symbol.t; span : Span.t }
exception Missing_record_field of { value : Symbol.t }
exception Missing_tuple_index of { value : int }
exception Constrain_error of { lhs : Simple_type.t; rhs : Simple_type.t }

exception
  Type_error of {
    expected : Simple_type.t;
    actual : Simple_type.t;
    span : Span.t;
  }

exception
  Slice_of_vector_error of { value : Symbol.t; span : Span.t; message : string }

type read_or_write = Read | Write | Writeonly

exception Readability_error of { value : Simple_type.t; rw : read_or_write }

exception
  Invalid_operation of {
    op : Syntax.Op.t;
    args : Simple_type.t list;
    span : Span.t;
  }

module type FRESH_VAR = sig
  val f : Level.t -> Simple_type.t
end

let create_fresh_vars ~fresher =
  let counter = ref 0 in
  let f level =
    let v = !counter in
    counter := v + 1;
    Variable.create ~level ~fresher
  in
  (module struct
    let f = f
  end : FRESH_VAR)

module type SCHEME = sig
  type t = private Sch_simple of Simple_type.t | Sch_poly of PolymorhicType.t

  val of_simple_type : Simple_type.t -> t
  val of_polymorphic_type : PolymorhicType.t -> t
  val instantiate : t -> Level.t -> Simple_type.t
  val level : t -> Level.t
end

module rec Scheme : functor (Fresh_var : FRESH_VAR) -> SCHEME =
functor
  (Fresh_var : FRESH_VAR)
  ->
  struct
    type t = Sch_simple of Simple_type.t | Sch_poly of PolymorhicType.t

    let of_simple_type t = Sch_simple t
    let of_polymorphic_type t = Sch_poly t

    let instantiate t lvl =
      let open Simple_type in
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
              let type_level = Simple_type.level ty in
              if Level.compare type_level limit < 1 then ty
              else
                match ty with
                | Smutable { read; write; scope } ->
                    Smutable
                      { read = freshen read; write = freshen write; scope }
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
                | Ssparse_tuple { indices } ->
                    Ssparse_tuple
                      {
                        indices =
                          List.map indices ~f:(fun (i, t) -> (i, freshen t));
                      }
                | Stuple_type { first; second; rest } ->
                    Stuple_type
                      {
                        first = freshen first;
                        second = freshen second;
                        rest = List.map rest ~f:freshen;
                      }
                | Sreference { read; write; scope } ->
                    let read = Option.map read ~f:freshen in
                    let write = Option.map write ~f:freshen in
                    Sreference { read; write; scope }
                | Svector_type { read; write; scope } ->
                    let read = Option.map read ~f:freshen in
                    let write = Option.map write ~f:freshen in
                    Svector_type { read; write; scope }
                | Srecord { fields } ->
                    Srecord
                      {
                        fields =
                          List.map fields ~f:(fun (f, t) -> (f, freshen t));
                      }
                | Sint_type -> Sint_type
                | Sbool_type -> Sbool_type
                | Sfloat_type -> Sfloat_type
                | Sunit_type -> Sunit_type
            in
            freshen ty
          in
          freshen_above limit body lvl

    and level = function
      | Sch_simple t -> Simple_type.level t
      | Sch_poly (PolymorhicType { level; _ }) -> level
  end

module Extrude = struct
  module type T = sig
    val f : Polar.Type.t -> Simple_type.t
  end

  module Self = struct
    type t = Self of { level : Level.t }

    let create level = Self { level }
    let level (Self { level; _ }) = level
  end

  module Make (S : sig
    module Fresh_sym : FRESH_SYM

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
        let open Simple_type in
        let polarity = Polar.Type.polarity t in
        let create_type t = Polar.Type.create t polarity in
        match Polar.Type.type_of t with
        | Sint_type -> Sint_type
        | Sfloat_type -> Sfloat_type
        | Sbool_type -> Sbool_type
        | Sunit_type -> Sunit_type
        | Smutable { read; write; scope } ->
            let read = extrude self (create_type read) in
            let write =
              extrude self (Polar.Type.create write (Polar.not polarity))
            in
            Smutable { read; write; scope }
        | Sfunction_type { argument; result } ->
            Sfunction_type
              {
                argument =
                  extrude self (Polar.Type.create argument (Polar.not polarity));
                result = extrude self (create_type result);
              }
        | Ssparse_tuple { indices } ->
            Ssparse_tuple
              {
                indices =
                  List.map indices ~f:(fun (i, t) ->
                      (i, extrude self (create_type t)));
              }
        | Stuple_type { first; second; rest } ->
            Stuple_type
              {
                first = extrude self (create_type first);
                second = extrude self (create_type second);
                rest = List.map rest ~f:(fun t -> extrude self (create_type t));
              }
        | Sreference { read; write; scope } ->
            let read =
              Option.map read ~f:(fun read -> extrude self (create_type read))
            in
            let write =
              Option.map write ~f:(fun write ->
                  extrude self (Polar.Type.create write (Polar.not polarity)))
            in
            Sreference { read; write; scope }
        | Svector_type { read; write; scope } ->
            let read =
              Option.map read ~f:(fun read -> extrude self (create_type read))
            in
            let write =
              Option.map write ~f:(fun write ->
                  extrude self (Polar.Type.create write (Polar.not polarity)))
            in
            Svector_type { read; write; scope }
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
                let nvs =
                  Variable.create ~level:(Self.level self)
                    ~fresher:(module S.Fresh_sym)
                in
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
    val f : Simple_type.t -> Simple_type.t -> unit
  end

  module Make (Fresh_sym : FRESH_SYM) : T = struct
    module Constraint = struct
      module T = struct
        type t = Constraint of { lhs : Simple_type.t; rhs : Simple_type.t }
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

        module Fresh_sym = Fresh_sym
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
        | Sfloat_type, Sfloat_type -> ()
        | Sbool_type, Sbool_type -> ()
        | Sunit_type, Sunit_type -> ()
        | Smutable _, Smutable _ -> failwith "mutable type is not allowed yet"
        | Smutable _, _ ->
            failwith "mutable type is not allowed yet (mutable is lhs)"
        | lhs, Smutable { write = rhs; read = _; _ } ->
            constrain (Constraint { lhs; rhs })
        | ( Sfunction_type { argument = lhs_arg; result = lhs_res },
            Sfunction_type { argument = rhs_arg; result = rhs_res } ) ->
            constrain (Constraint { lhs = rhs_arg; rhs = lhs_arg });
            constrain (Constraint { lhs = lhs_res; rhs = rhs_res })
        | ( Stuple_type
              { first = lhs_first; second = lhs_second; rest = lhs_rest },
            Stuple_type
              { first = rhs_first; second = rhs_second; rest = rhs_rest } ) ->
            constrain (Constraint { lhs = lhs_first; rhs = rhs_first });
            constrain (Constraint { lhs = lhs_second; rhs = rhs_second });
            List.iter2_exn lhs_rest rhs_rest ~f:(fun lhs rhs ->
                constrain (Constraint { lhs; rhs }))
        | Ssparse_tuple { indices = lhs }, Ssparse_tuple { indices = rhs } ->
            List.iter rhs ~f:(fun (j, rhs_type) ->
                match List.Assoc.find lhs j ~equal:Int.equal with
                | Some lhs_type ->
                    constrain (Constraint { lhs = lhs_type; rhs = rhs_type })
                | None -> raise (Missing_tuple_index { value = j }))
        | ( Stuple_type
              { first = lhs_first; second = lhs_second; rest = lhs_rest },
            Ssparse_tuple { indices = rhs } ) ->
            List.iter rhs ~f:(fun (j, rhs_type) ->
                match j with
                | 0 ->
                    constrain (Constraint { lhs = lhs_first; rhs = rhs_type })
                | 1 ->
                    constrain (Constraint { lhs = lhs_second; rhs = rhs_type })
                | n -> (
                    let i = n - 2 in
                    match List.nth lhs_rest i with
                    | Some lhs_type ->
                        constrain
                          (Constraint { lhs = lhs_type; rhs = rhs_type })
                    | None -> raise (Missing_tuple_index { value = i })))
        | ( Ssparse_tuple { indices = lhs },
            Stuple_type
              { first = rhs_first; second = rhs_second; rest = rhs_rest } ) ->
            List.iter lhs ~f:(fun (j, lhs_type) ->
                match j with
                | 0 ->
                    constrain (Constraint { lhs = lhs_type; rhs = rhs_first })
                | 1 ->
                    constrain (Constraint { lhs = lhs_type; rhs = rhs_second })
                | n -> (
                    let i = n - 2 in
                    match List.nth rhs_rest i with
                    | Some rhs_type ->
                        constrain
                          (Constraint { lhs = lhs_type; rhs = rhs_type })
                    | None -> raise (Missing_tuple_index { value = i })))
        | ( Sreference { read = lhs_read; write = lhs_write; _ },
            Sreference { read = rhs_read; write = rhs_write; _ } ) -> (
            (match (lhs_read, rhs_read) with
            | Some lhs_read, Some rhs_read ->
                constrain (Constraint { lhs = lhs_read; rhs = rhs_read })
            | None, Some _ ->
                raise (Readability_error { value = rhs; rw = Read })
            | _ -> ());
            match (lhs_write, rhs_write) with
            | Some lhs_write, Some rhs_write ->
                constrain (Constraint { lhs = rhs_write; rhs = lhs_write })
            | None, Some rhs ->
                raise (Readability_error { value = rhs; rw = Write })
            | _ -> ())
        | ( Svector_type { read = lhs_read; write = lhs_write; _ },
            Svector_type { read = rhs_read; write = rhs_write; _ } ) -> (
            (match (lhs_read, rhs_read) with
            | Some lhs_read, Some rhs_read ->
                constrain (Constraint { lhs = lhs_read; rhs = rhs_read })
            | None, Some _ ->
                raise (Readability_error { value = rhs; rw = Read })
            | _ -> ());
            match (lhs_write, rhs_write) with
            | Some lhs_write, Some rhs_write ->
                constrain (Constraint { lhs = rhs_write; rhs = lhs_write })
            | None, Some rhs ->
                raise (Readability_error { value = rhs; rw = Write })
            | _ -> ())
        | Srecord { fields = lhs_fields }, Srecord { fields = rhs_fields } ->
            List.iter rhs_fields ~f:(fun (rhs_field, rhs_type) ->
                match
                  List.Assoc.find lhs_fields rhs_field ~equal:Symbol.equal
                with
                | Some lhs_type ->
                    constrain (Constraint { lhs = lhs_type; rhs = rhs_type })
                | None -> raise (Missing_record_field { value = rhs_field }))
        | Svar_type { state = lhs_state }, rhs
          when Simple_type.level rhs <= Variable.level lhs_state ->
            Variable.upper_bounds lhs_state
            := rhs :: !(Variable.upper_bounds lhs_state);
            List.iter
              !(Variable.lower_bounds lhs_state)
              ~f:(fun lhs -> constrain (Constraint { lhs; rhs }))
        | lhs, Svar_type { state = rhs_state }
          when Simple_type.level lhs <= Variable.level rhs_state ->
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
        | lhs, rhs ->
            (* let string =
                 Printf.sprintf "lhs: %s rhs: %s"
                   (Simple_type.to_string lhs)
                   (Simple_type.to_string rhs)
               in
               print_endline string; *)
            raise (Constrain_error { lhs; rhs }))
      else ()
  end
end

module type MAPPER = sig
  type self

  val create : unit -> self
  val map : Syntax.t -> Tast.t Or_error.t
  val map_ast : self -> Syntax.t -> Tast.t
  val map_closure : self -> Syntax.Closure.t -> Closure.t
  val map_iterate : self -> Syntax.iterate -> self * Tast.Iterate.t
end

module type S = sig
  module Fresh_var : FRESH_VAR
  module Fresh_sym : FRESH_SYM
end

module rec Make : functor (S : S) -> MAPPER =
functor
  (S : S)
  ->
  struct
    module TypeScheme = Scheme (S.Fresh_var)

    module Self = struct
      type t =
        | Self of {
            self : (Symbol.t, TypeScheme.t, Symbol.comparator_witness) Map.t;
            level : Level.t;
            scope : Scope.t;
          }

      let create () =
        Self
          {
            self = Map.empty (module Symbol);
            level = Level.default;
            scope = Scope.default;
          }

      let contains (Self { self; _ }) v = Map.mem self v
      let level (Self { level; _ }) = level

      let level_up (Self { self; level; scope }) =
        Self { self; level = Level.level_up level; scope }

      let find_exn (Self { self; _ }) v = Map.find_exn self v

      let add (Self { self; level; scope }) ~key ~data =
        Self { self = Map.add_exn self ~key ~data; level; scope }

      let incr_scope (Self { self; level; scope }) =
        Self { self; level; scope = Scope.incr scope }

      let scope (Self { scope; _ }) = scope
    end

    type self = Self.t

    let create () = Self.create ()

    let constrain lhs rhs =
      let module C = Constrain.Make (S.Fresh_sym) in
      C.f lhs rhs

    let rec map_ast self =
      let open Simple_type in
      let open Syntax in
      let open Tast.T in
      let open Op in
      function
      | Uint { value; span } -> Tint { value; span }
      | Ufloat { value; span } -> Tfloat { value; span }
      | Ubool { value; span } -> Tbool { value; span }
      | Uneg { value; span } ->
          let value = map_ast self value in
          constrain (Tast.T.type_of value) Sint_type;
          Tprimop { op = Tint_neg; args = [ value ]; span; type' = Sint_type }
      | Uop { op = (Add | Sub | Mul | Div) as int_op; left; right; span } ->
          let left = map_ast self left in
          let right = map_ast self right in
          constrain (Tast.T.type_of left) Sint_type;
          constrain (Tast.T.type_of right) Sint_type;
          let op =
            let open Primop in
            match int_op with
            | Add -> Tint_add
            | Sub -> Tint_sub
            | Mul -> Tint_mul
            | Div -> Tint_div
            | _ -> assert false
          in
          Tprimop { op; args = [ left; right ]; span; type' = Sint_type }
      | Uop { op = (FAdd | FSub | FMul | FDiv) as float_op; left; right; span }
        ->
          let left = map_ast self left in
          let right = map_ast self right in
          constrain (Tast.T.type_of left) Sfloat_type;
          constrain (Tast.T.type_of right) Sfloat_type;
          let op =
            let open Primop in
            match float_op with
            | FAdd -> Tfloat_add
            | FSub -> Tfloat_sub
            | FMul -> Tfloat_mul
            | FDiv -> Tfloat_div
            | _ -> assert false
          in
          Tprimop { op; args = [ left; right ]; span; type' = Sfloat_type }
      (* NOTE: not eq and cmp are different concepts *)
      | Uop
          { op = (Eq | Neq | Geq | Leq | Lt | Gt) as relop; left; right; span }
        ->
          let left = map_ast self left in
          let right = map_ast self right in

          constrain (Tast.T.type_of left) (Tast.T.type_of right);
          let op =
            let open Primop in
            let lhs = Tast.T.type_of left in
            let rhs = Tast.T.type_of right in
            match (relop, lhs, rhs) with
            | Eq, Sint_type, Sint_type -> Tint_eq
            | Neq, Sint_type, Sint_type -> Tint_ne
            | Geq, Sint_type, Sint_type -> Tint_ge
            | Leq, Sint_type, Sint_type -> Tint_le
            | Lt, Sint_type, Sint_type -> Tint_lt
            | Gt, Sint_type, Sint_type -> Tint_gt
            | Eq, Sfloat_type, Sfloat_type -> Tfloat_eq
            | Neq, Sfloat_type, Sfloat_type -> Tfloat_ne
            | Geq, Sfloat_type, Sfloat_type -> Tfloat_ge
            | Leq, Sfloat_type, Sfloat_type -> Tfloat_le
            | Lt, Sfloat_type, Sfloat_type -> Tfloat_lt
            | Gt, Sfloat_type, Sfloat_type -> Tfloat_gt
            | Eq, Sbool_type, Sbool_type -> Tbool_eq
            | Neq, Sbool_type, Sbool_type -> Tbool_ne
            | _ -> assert false
          in
          Tprimop { op; args = [ left; right ]; span; type' = Sbool_type }
      | Uvar { value; span } when Self.contains self value ->
          let type' = Self.find_exn self value in
          let type' = TypeScheme.instantiate type' (Self.level self) in
          let type' =
            match type' with
            | Sreference { read = Some read; scope; _ } ->
                Sreference { read = Some read; write = None; scope }
            | Smutable { read; _ } -> read
            | _ -> type'
          in
          Tvar { value; span; type' }
      | Uvar { value; span } -> raise (Unbound_variable { value; span })
      | Utuple { values = []; span } -> Tunit { span }
      | Utuple { values = [ value ]; _ } -> map_ast self value
      | Utuple { values; span } ->
          let values = List.map values ~f:(map_ast self) in
          let first, second, rest =
            match values with
            | first :: second :: rest -> (first, second, rest)
            | _ -> assert false
          in
          Ttuple { first; second; rest; span }
      | Uvector { values; span; mutability = Mutability.Immutable } ->
          let values = List.map values ~f:(map_ast self) in

          let read = S.Fresh_var.f (Self.level self) in
          List.iter values ~f:(fun value ->
              constrain (Tast.T.type_of value) read);
          let type' =
            Svector_type
              { read = Some read; write = None; scope = Self.scope self }
          in
          Tvector { values; span; type' }
      | Uvector { values; span; mutability = Mutability.Mutable } ->
          let values = List.map values ~f:(map_ast self) in
          let write = S.Fresh_var.f (Self.level self) in
          let read = S.Fresh_var.f (Self.level self) in
          List.iter values ~f:(fun value ->
              constrain (Tast.T.type_of value) read);
          let type' =
            Svector_type
              { read = Some read; write = Some write; scope = Self.scope self }
          in
          Tvector { values; span; type' }
      | Uvector { values; span; mutability = Mutability.Reference } ->
          let values = List.map values ~f:(map_ast self) in
          let write = S.Fresh_var.f (Self.level self) in
          let read = S.Fresh_var.f (Self.level self) in
          List.iter values ~f:(fun value ->
              constrain (Tast.T.type_of value) read);
          let type' =
            Svector_type
              { read = Some read; write = Some write; scope = Scope.Global }
          in
          Tvector { values; span; type' }
      | Uvector { mutability = Mutability.MutableReference; _ } ->
          (* This will never be parsed *)
          assert false
      | Uslice { value; readability = Readability.Readonly; span }
        when Self.contains self value ->
          let type' =
            let scheme = Self.find_exn self value in
            match TypeScheme.instantiate scheme (Self.level self) with
            | Svector_type
                { read = Some read; write = Some _; scope = Scope.Global } ->
                Svector_type
                  { read = Some read; write = None; scope = Scope.default }
            | Svector_type { read = Some _; write = None; _ } ->
                raise
                  (Slice_of_vector_error
                     { value; span; message = "vector is already readonly" })
            | Svector_type { read = Some _; write = Some _; _ } ->
                raise
                  (Slice_of_vector_error
                     {
                       value;
                       span;
                       message =
                         "mutable vectors cannot explicitly be made readonly";
                     })
            | ty ->
                let level = Self.level self in
                let read = S.Fresh_var.f level in
                let write = S.Fresh_var.f level in
                let expected =
                  Sreference
                    {
                      read = Some read;
                      write = Some write;
                      scope = Scope.Global;
                    }
                in
                raise (Type_error { expected; actual = ty; span })
          in
          Tvar { value; span; type' }
      | Uslice { value; readability = Readability.ReadWrite; span }
        when Self.contains self value ->
          let type' =
            let scheme = Self.find_exn self value in
            match TypeScheme.instantiate scheme (Self.level self) with
            | Svector_type
                { read = Some read; write = Some write; scope = Scope.Global }
              ->
                Svector_type
                  { read = Some read; write = Some write; scope = Scope.Global }
            | Svector_type { read = Some _; write = None; _ } ->
                raise
                  (Slice_of_vector_error
                     { value; span; message = "vector is readonly" })
            | Svector_type { read = Some _; write = Some _; _ } ->
                raise
                  (Slice_of_vector_error
                     {
                       value;
                       span;
                       message = "mutable vectors are already readwrite locally";
                     })
            | ty ->
                let level = Self.level self in
                let read = S.Fresh_var.f level in
                let write = S.Fresh_var.f level in
                let expected =
                  Svector_type
                    {
                      read = Some read;
                      write = Some write;
                      scope = Scope.Global;
                    }
                in
                raise (Type_error { expected; actual = ty; span })
          in
          Tvar { value; span; type' }
      | Uslice { readability = Readability.Writeonly; _ } ->
          failwith "slices cannot be writeonly without annotations"
      | Uslice { value; span; _ } -> raise (Unbound_variable { value; span })
      | Utuple_subscript { value; index; span } ->
          let value = map_ast self value in
          let res = S.Fresh_var.f (Self.level self) in
          constrain (Tast.T.type_of value)
            (Ssparse_tuple { indices = [ (index, res) ] });
          Ttuple_subscript { value; index; span; type' = res }
      | Usubscript { value; index; span } when Self.contains self value ->
          let scheme = Self.find_exn self value in
          let type' = TypeScheme.instantiate scheme (Self.level self) in
          let index = map_ast self index in
          let res = S.Fresh_var.f (Self.level self) in
          constrain type'
            (Simple_type.Svector_type
               { read = Some res; write = None; scope = Self.scope self });
          constrain (Tast.T.type_of index) Sint_type;
          Tsubscript { value; index; span; type' = res }
      | Usubscript { value; span; _ } ->
          raise (Unbound_variable { value; span })
      | Uassign { name; value; span } when Self.contains self name ->
          let type' =
            let scheme = Self.find_exn self name in
            match TypeScheme.instantiate scheme (Self.level self) with
            | Smutable { scope; write; _ } when Scope.(scope = Self.scope self)
              ->
                write
            | Smutable { write; read; _ } as actual ->
                let expected =
                  Smutable { scope = Self.scope self; write; read }
                in
                raise (Type_error { expected; actual; span })
            | ty ->
                let level = Self.level self in
                let read = S.Fresh_var.f level in
                let write = S.Fresh_var.f level in
                let scope = Self.scope self in
                let expected = Smutable { read; write; scope } in
                raise (Type_error { expected; actual = ty; span })
          in
          let value = map_ast self value in
          constrain (Tast.T.type_of value) type';
          Tassign { name = (name, type'); value; span }
      | Uassign { name = value; span; _ } ->
          raise (Unbound_variable { value; span })
      | Uderef { name; span } when Self.contains self name ->
          let scheme = Self.find_exn self name in
          let type' = TypeScheme.instantiate scheme (Self.level self) in
          let res = S.Fresh_var.f (Self.level self) in
          constrain type'
            (Simple_type.Sreference
               { read = Some res; write = None; scope = Self.scope self });
          Tderef { name; type' = res; span }
      | Uderef { name = value; span; _ } ->
          raise (Unbound_variable { value; span })
      | Uupdate_ref { name; value; span } when Self.contains self name ->
          let value = map_ast self value in
          let value_type = Tast.T.type_of value in
          let type' =
            let scheme = Self.find_exn self name in
            match TypeScheme.instantiate scheme (Self.level self) with
            | Sreference { scope; write = Some write; _ }
              when Scope.(scope = Self.scope self) ->
                write
            | Sreference { write = Some write; scope = Scope.Global; _ } ->
                write
            | ty ->
                let expected =
                  Sreference
                    {
                      read = None;
                      write = Some value_type;
                      scope = Scope.Global;
                    }
                in
                raise (Type_error { expected; actual = ty; span })
          in

          constrain value_type type';
          Tupdate_ref { name; value; span }
      | Uupdate_ref { name = value; span; _ } ->
          raise (Unbound_variable { value; span })
      | Umutable_ref { value; span } when Self.contains self value ->
          let type' =
            let scheme = Self.find_exn self value in
            match TypeScheme.instantiate scheme (Self.level self) with
            | Sreference
                { read = Some read; write = Some write; scope = Scope.Global }
              ->
                Sreference
                  { read = Some read; write = Some write; scope = Scope.Global }
            | Sreference { read = Some read; write = None; scope } as actual ->
                let variable = S.Fresh_var.f (Self.level self) in
                let expected =
                  Sreference { read = Some read; write = Some variable; scope }
                in
                raise (Type_error { expected; actual; span })
            | Sreference { read = Some read; write = Some write; _ } as actual
              ->
                let expected =
                  Sreference
                    {
                      read = Some read;
                      write = Some write;
                      scope = Scope.Global;
                    }
                in
                raise (Type_error { expected; actual; span })
            | ty ->
                let level = Self.level self in
                let read = S.Fresh_var.f level in
                let write = S.Fresh_var.f level in
                let expected =
                  Sreference
                    {
                      read = Some read;
                      write = Some write;
                      scope = Scope.Global;
                    }
                in
                raise (Type_error { expected; actual = ty; span })
          in
          Tvar { value; span; type' }
      | Umutable_ref { value; span } -> raise (Unbound_variable { value; span })
      | Uassign_subscript { name; index; value; span }
        when Self.contains self name ->
          let value = map_ast self value in
          let value_type = Tast.T.type_of value in
          let type' =
            let scheme = Self.find_exn self name in
            match TypeScheme.instantiate scheme (Self.level self) with
            | Svector_type { scope = Scope.Global; write = Some write; _ } ->
                write
            | Svector_type { scope; write = Some write; _ }
              when Scope.(scope = Self.scope self) ->
                write
            | ty ->
                raise
                  (Type_error
                     {
                       expected =
                         Simple_type.Svector_type
                           {
                             read = None;
                             write = Some value_type;
                             scope = Self.scope self;
                           };
                       actual = ty;
                       span;
                     })
          in

          let index = map_ast self index in

          constrain (Tast.T.type_of index) Sint_type;
          constrain value_type type';
          Tassign_subscript { name; index; value; span }
      | Uassign_subscript { name; span; _ } ->
          raise (Unbound_variable { value = name; span })
      | Urecord { fields; span } ->
          Trecord
            {
              fields =
                List.map fields ~f:(fun (field, e) -> (field, map_ast self e));
              span;
            }
      | Uselect { value; field; span } ->
          let res = S.Fresh_var.f (Self.level self) in
          let value = map_ast self value in
          constrain (Tast.T.type_of value)
            (Simple_type.Srecord { fields = [ (field, res) ] });
          Tselect { value; field; span; type' = res }
      | Ulambda { closure } -> Tlambda { closure = map_closure self closure }
      | Ulet_fun { name; closure; app; span } ->
          let value = map_closure (Self.level_up self) closure in
          let level = Self.level self in
          let type' = Tast.Closure.type_of value in
          let type' = PolymorhicType.create level type' in
          let scheme = TypeScheme.of_polymorphic_type type' in
          let app = map_ast (Self.add self ~key:name ~data:scheme) app in
          let type' = TypeScheme.instantiate scheme level in
          Tlet
            {
              binding = (name, type');
              value = Tlambda { closure = value };
              app;
              span;
            }
      | Ulet
          {
            binding;
            mutability = Mutability.Immutable;
            value = Ulambda { closure };
            app;
            span;
          } ->
          map_ast self (Ulet_fun { name = binding; closure; app; span })
      | Ulet { binding; mutability = Mutability.Reference; value; app; span } ->
          let value = map_ast (Self.level_up self) value in
          let level = Self.level self in
          let type' = Tast.T.type_of value in
          let read = S.Fresh_var.f level in
          let write = S.Fresh_var.f level in
          constrain type' read;
          constrain type' write;
          let type' =
            Simple_type.Sreference
              { read = Some read; write = Some write; scope = Self.scope self }
          in
          let scheme = TypeScheme.of_simple_type type' in
          let app = map_ast (Self.add self ~key:binding ~data:scheme) app in
          let type' = TypeScheme.instantiate scheme level in
          Tlet { binding = (binding, type'); value; app; span }
      | Ulet
          {
            binding;
            mutability = Mutability.MutableReference;
            value;
            app;
            span;
          } ->
          let value = map_ast (Self.level_up self) value in
          let level = Self.level self in
          let type' = Tast.T.type_of value in
          let read = S.Fresh_var.f level in
          let write = S.Fresh_var.f level in
          constrain type' read;
          constrain type' write;
          let type' =
            Simple_type.Sreference
              { read = Some read; write = Some write; scope = Scope.global }
          in
          let scheme = TypeScheme.of_simple_type type' in
          let app = map_ast (Self.add self ~key:binding ~data:scheme) app in
          let type' = TypeScheme.instantiate scheme level in
          Tlet { binding = (binding, type'); value; app; span }
      | Ulet { binding; mutability = Mutability.Mutable; value; app; span } ->
          let value = map_ast (Self.level_up self) value in
          let level = Self.level self in
          let type' = Tast.T.type_of value in
          let read = S.Fresh_var.f level in
          let write = S.Fresh_var.f level in
          constrain type' read;
          constrain type' write;
          let type' =
            Simple_type.Smutable { read; write; scope = Self.scope self }
          in
          let scheme = TypeScheme.of_simple_type type' in
          let app = map_ast (Self.add self ~key:binding ~data:scheme) app in
          let type' = TypeScheme.instantiate scheme level in
          Tlet { binding = (binding, type'); value; app; span }
      | Ulet { binding; value; app; span; mutability = Mutability.Immutable } ->
          let value = map_ast (Self.level_up self) value in
          let level = Self.level self in
          let type' = Tast.T.type_of value in
          let scheme = TypeScheme.of_simple_type type' in
          let app = map_ast (Self.add self ~key:binding ~data:scheme) app in
          let type' = TypeScheme.instantiate scheme level in
          Tlet { binding = (binding, type'); value; app; span }
      | Useq { first; second; span } ->
          let first = map_ast self first in
          let second = map_ast self second in
          Tseq { first; second; span }
      | Udef { name; closure; app; span } ->
          let expr = S.Fresh_var.f (Level.level_up (Self.level self)) in
          let closure =
            let type' = TypeScheme.of_simple_type expr in
            let self = Self.add self ~key:name ~data:type' in
            let self = Self.level_up self in
            map_closure self closure
          in
          constrain (Tast.Closure.type_of closure) expr;
          let self =
            let poly_type = PolymorhicType.create (Self.level self) expr in
            Self.add self ~key:name
              ~data:(TypeScheme.of_polymorphic_type poly_type)
          in
          let app = map_ast self app in
          let fn_type =
            TypeScheme.instantiate (Self.find_exn self name) (Self.level self)
          in
          Tdef { name; closure; app; span; fn_type }
      | Uapp { fn; value; span } ->
          let res = S.Fresh_var.f (Self.level self) in
          let fn = map_ast self fn in
          let arg = map_ast self value in
          constrain (Tast.T.type_of fn)
            (Simple_type.Sfunction_type
               { argument = Tast.T.type_of arg; result = res });
          Tapp { fn; value = arg; span; type' = res }
      | Uif_end { cond; then_; span } ->
          map_ast self
            (Uif { cond; then_; else_ = Utuple { values = []; span }; span })
      | Uif { cond; then_; else_; span } ->
          let cond = map_ast (Self.level_up self) cond in
          constrain (Tast.T.type_of cond) Sbool_type;
          let type' = S.Fresh_var.f (Self.level self) in
          let then_ = map_ast self then_ in
          constrain (Tast.T.type_of then_) type';
          let else_ = map_ast self else_ in
          constrain (Tast.T.type_of else_) type';
          Tif { cond; then_; else_; span; type' }
      | Ufor { iterates; body; span } ->
          let self, iterate = map_iterate self iterates in
          let body = map_ast self body in
          constrain (Tast.T.type_of body) Sunit_type;
          Tfor { iterate; body; span; type' = Sunit_type }

    and map_iterate self iterate =
      let rec loop self iter =
        let open Syntax in
        let open Tast.Iterate in
        match iter with
        | Udone -> (self, Tdone)
        | Uiterate { name; start; finish; is_ascending; span; rest } ->
            let start = map_ast (Self.level_up self) start in
            constrain (Tast.T.type_of start) Sint_type;
            let finish = map_ast (Self.level_up self) finish in
            constrain (Tast.T.type_of finish) Sint_type;
            let type' = PolymorhicType.create (Self.level self) Sint_type in
            let scheme = TypeScheme.of_polymorphic_type type' in
            let self = Self.add self ~key:name ~data:scheme in
            let self, rest = loop self rest in
            let type' = TypeScheme.instantiate scheme (Self.level self) in
            ( self,
              Titerate
                {
                  name = (name, type');
                  start;
                  finish;
                  is_ascending;
                  span;
                  rest;
                } )
      in
      loop self iterate

    and map_closure self (Syntax.Uclosure { parameter; value; span }) =
      let self = Self.incr_scope self in
      let type' = S.Fresh_var.f (Self.level self) in

      let self =
        let type' = TypeScheme.of_simple_type type' in
        Self.add self ~key:parameter ~data:type'
      in
      Tclosure
        { parameter = (parameter, type'); value = map_ast self value; span }

    let map ast =
      Or_error.try_with (fun () ->
          let self = create () in
          map_ast self ast)
  end

module Tests = struct
  let run_it s =
    match
      (let open Or_error.Let_syntax in
       let%bind ast = To_syntax.parse s in
       let (module Fresh_sym) = Text.create_fresh_sym () in
       let (module Fresh_var) =
         (module struct
           let counter = ref 0

           let f level =
             let v = !counter in
             counter := v + 1;
             Variable.create ~level ~fresher:(module Fresh_sym)
         end : FRESH_VAR)
       in
       let (module S) =
         (module struct
           module Fresh_sym = Fresh_sym
           module Fresh_var = Fresh_var
         end : S)
       in
       let (module Visitor) = (module Make (S) : MAPPER) in
       let%bind t = Visitor.map ast in
       t |> Tast.T.type_of |> Simple_type.to_string |> Ok)
      |> Or_error.ok_exn |> print_endline
    with
    | () -> ()
    | exception Type_error { expected; actual; span } ->
        print_endline
          (Printf.sprintf "Type error\n  expected: %s\n  actual: %s\n  span: %s"
             (Simple_type.to_string expected)
             (Simple_type.to_string actual)
             (Span.to_string span))
    | exception exn -> print_endline (Exn.to_string exn)

  let%expect_test "type empty file" =
    run_it {| |};
    [%expect {| () |}]

  let%expect_test "type int" =
    run_it {| 1 |};
    [%expect {| int |}]

  let%expect_test "type neg int" =
    run_it {| -1 |};
    [%expect {| int |}]

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
    [%expect {| int <: '__0 |}]

  let%expect_test "type lambda" =
    run_it {| fn x -> 10 |};
    [%expect {| '__0 -> int |}]

  let%expect_test "type let" =
    run_it {| let x = 0 in x|};
    [%expect {| int |}]

  let%expect_test "type let function" =
    run_it {| let f x -> 0 in f |};
    [%expect {| '__1 -> int |}]

  let%expect_test "type app" =
    run_it {| let f x -> 0 in f 2 |};
    [%expect {| int <: '__1 |}]

  let%expect_test "type def function" =
    run_it {|
      def f x = f 0 in 
      f
    |};
    [%expect {| '__3 |}]

  let%expect_test "type binop" =
    run_it "1 + 2";
    [%expect {| int |}]

  let%expect_test "type float binop" =
    run_it "1.0 +. 2.0";
    [%expect {| float |}]

  let%expect_test "type binop precedence" =
    run_it "1 + 2 * 3";
    [%expect {| int |}]

  let%expect_test "type relop" =
    run_it "1 < 2";
    [%expect {| bool |}]

  let%expect_test "type float relop" =
    run_it "1.0 < 2.0";
    [%expect {| bool |}]

  let%expect_test "type binop paren" =
    run_it "(1 + 2) * 3";
    [%expect {| int |}]

  let%expect_test "bad binop" =
    run_it "1 + 0.67";
    [%expect {| ("Fx__Typing.Constrain_error(1, 0)") |}]

  let%expect_test "bool eq" =
    run_it "true == false";
    [%expect {| bool |}]

  let%expect_test "if end" =
    run_it "if 1 < 2 then 4 end";
    [%expect {| () | int <: '__0 |}]

  let%expect_test "if" =
    run_it "if true then 1 else 0";
    [%expect {| int | int <: '__0 |}]

  let%expect_test "if union" =
    run_it "if true then 1 else 0.0";
    [%expect {| float | int <: '__0 |}]

  let%expect_test "type paren" =
    run_it "(1)";
    [%expect {| int |}]

  let%expect_test "type tuple" =
    run_it "(1, 2)";
    [%expect {| (int, int) |}]

  let%expect_test "tuple single" =
    run_it "(1,)";
    [%expect {| (Fx__Parser.MenhirBasics.Error) |}]

  let%expect_test "tuple index" =
    run_it "(1, 2).0";
    [%expect {| int <: '__0 |}]

  let%expect_test "triple index" =
    run_it "(1, 2, true).2";
    [%expect {| bool <: '__0 |}]

  let%expect_test "empty vector" =
    run_it "[||]";
    [%expect {| vector['__0, _] @ 0 |}]

  let%expect_test "vector" =
    run_it "[| 1, 2 |]";
    [%expect {| vector[int | int <: '__0, _] @ 0 |}]

  let%expect_test "union vector" =
    run_it "[| 1, true |]";
    [%expect {| vector[bool | int <: '__0, _] @ 0 |}]

  let%expect_test "vector subscript" =
    run_it "let xs = [| 1, 2 |] in xs[0]";
    [%expect {| int <: '__1 |}]

  let%expect_test "union vector subscript" =
    run_it "let xs = [| true, 2 |] in xs[0]";
    [%expect {| bool | int <: '__1 |}]

  let%expect_test "let mut" =
    run_it "let mut x = 1 in x";
    [%expect {| int <: '__0 |}]

  let%expect_test "mut update" =
    run_it "x = 1";
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "vector update" =
    run_it "xs[0] = 1";
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "readonly vector" =
    run_it {|
      let xs = [| 0, 1|] in
      xs[0] = 1 |};
    [%expect
      {|
     Type error
       expected: vector[_, int] @ 0
       actual: vector[int | int <: '__0 @ 1, _] @ 0
       span: stdin:3:6:3:15 |}]

  let%expect_test "local readwrite vector" =
    run_it {|
       let xs = mut [| 0, 1|] in
       xs[0] = 1 |};
    [%expect {| () |}]

  let%expect_test "readwrite vector, no passing" =
    run_it {|
       let xs = ref [| 0, 1|] in
       xs[0] = 1 |};
    [%expect {| () |}]

  let%expect_test "readwrite vector, capture readonly" =
    run_it
      {|
       let xs = mut [| 0, 1|] in
       let f x -> xs[0] = x in
       xs[0] = 1;
       f 1|};
    [%expect
      {|
     Type error
       expected: vector[_, '__2 @ 1] @ 1
       actual: vector[int | int <: '__1 @ 1, '__0 @ 1] @ 0
       span: stdin:3:18:3:27 |}]

  let%expect_test "readwrite vector, capture readwrite" =
    run_it
      {|
       let xs = ref [| 0, 1|] in
       let f x -> xs[0] = x in
       xs[0] = 1;
       f 1|};
    [%expect {| () <: '__3 |}]

  let%expect_test "readonly vector" =
    run_it "xs[..]";
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "readwrite vector" =
    run_it "&mut xs[..]";
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "immutable vectors are already readonly" =
    run_it {|
      let xs = [| 0, 1|] in
      xs[..] |};
    [%expect
      {| ("Fx__Typing.Slice_of_vector_error(_, _, \"vector is already readonly\")") |}]

  let%expect_test "immutable vectors cannot be made writeable" =
    run_it {|
       let xs = [| 0, 1|] in
       &mut xs[..] |};
    [%expect
      {| ("Fx__Typing.Slice_of_vector_error(_, _, \"vector is readonly\")") |}]

  let%expect_test "mutable vectors can only be captured as readonly" =
    run_it {|
      let xs = mut [| 0, 1|] in
      xs[..] |};
    [%expect
      {| ("Fx__Typing.Slice_of_vector_error(_, _, \"mutable vectors cannot explicitly be made readonly\")") |}]

  let%expect_test "mutable vectors can only be captured as readonly" =
    run_it {|
      let xs = mut [| 0, 1|] in
      &mut xs[..] |};
    [%expect
      {| ("Fx__Typing.Slice_of_vector_error(_, _, \"mutable vectors are already readwrite locally\")") |}]

  let%expect_test "reference vectors can be captured as readonly" =
    run_it {|
      let xs = ref [| 0, 1|] in
      xs[..] |};
    [%expect {| vector[int | int <: '__1 @ 1, _] @ 0 |}]

  let%expect_test "reference vectors can be captured as readonly" =
    run_it {|
      let xs = ref [| 0, 1|] in
      &mut xs[..] |};
    [%expect {| vector[int | int <: '__1 @ 1, '__0 @ 1] |}]

  let%expect_test "readonly vectors cannot be written" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = xs[..] in
      ys[1] = 9|};
    [%expect
      {|
     Type error
       expected: vector[_, int] @ 0
       actual: vector[int | int <: '__1 @ 1, _] @ 0
       span: stdin:4:6:4:15 |}]

  let%expect_test "readwrite vectors can be read" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = &mut xs[..] in
      ys[1] |};
    [%expect {| int <: '__2 |}]

  let%expect_test "readonly vectors can be read" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = xs[..] in
      ys[1] |};
    [%expect {| int <: '__2 |}]

  let%expect_test "readwrite vectors be written" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = &mut xs[..] in
      ys[1] = 10 |};
    [%expect {| () |}]

  let%expect_test "captured readonly vectors cannot be written" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = xs[..] in
      let f x -> ys[1] = x in
      f () |};
    [%expect
      {|
       Type error
         expected: vector[_, '__2 @ 1] @ 1
         actual: vector[int | int <: '__1 @ 1, _] @ 0
         span: stdin:4:17:4:26 |}]

  let%expect_test "captured readwrite vectors can only be read" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = &mut xs[..] in
      let f x -> ys[1] in
      f () |};
    [%expect {| '__4 |}]

  let%expect_test "passed readonly vectors cannot be written" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let f ys -> ys[1] = 99 in
      f xs[..] |};
    [%expect
      {|
     Type error
       expected: vector[_, int] @ 1
       actual: '__2 @ 1
       span: stdin:3:18:3:28 |}]

  let%expect_test "passed writeonly vectors cannot be written without \
                   annotations" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let f ys -> ys[1] = 99 in
      f &mut xs[..] |};
    [%expect
      {|
     Type error
       expected: vector[_, int] @ 1
       actual: '__2 @ 1
       span: stdin:3:18:3:28 |}]

  let%expect_test "passed readonly vectors be read" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let f ys -> ys[1] in
      f xs[..] |};
    [%expect {| '__4 |}]

  let%expect_test "passed readonly vectors cannot be made writable" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let f ys -> 
        let xs = &mut ys[..] in
        xs[0] = 2 in
      f xs[..] |};
    [%expect
      {|
     Type error
       expected: vector['__3 @ 2, '__4 @ 2]
       actual: '__2 @ 1
       span: stdin:4:17:4:28 |}]

  let%expect_test "ref update" =
    run_it "a := 1";
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "deref" =
    run_it "!a";
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "let ref" =
    run_it "let ref a = 0 in a";
    [%expect {| ref[int <: '__0, _] @ 0 |}]

  let%expect_test "let ref update" =
    run_it "let ref a = 0 in a := 1";
    [%expect {| () |}]

  let%expect_test "let ref deref" =
    run_it "let ref a = 0 in !a";
    [%expect {| int <: '__2 |}]

  let%expect_test "let ref update union" =
    run_it {|
      let ref a = 0 in
      a := 1.0;
      a
    |};
    [%expect {| ref[int <: '__0, _] @ 0 |}]

  let%expect_test "let ref deref union" =
    run_it {|
      let ref a = 0 in
      a := 1.0;
      !a
    |};
    [%expect {| int <: '__2 |}]

  let%expect_test "seq" =
    run_it "1; 2";
    [%expect {| int |}]

  let%expect_test "let mut update" =
    run_it "let mut a = 0 in a = 1";
    [%expect {| () |}]

  let%expect_test "mut capture by value" =
    run_it {| 
      let mut a = 0 in
      let f x -> a = x in
      f 1 |};
    [%expect
      {|
     Type error
       expected: mut[int <: '__0, int <: '__1] @ 1
       actual: mut[int <: '__0, int <: '__1] @ 0
       span: stdin:3:17:3:22 |}]

  let%expect_test "declare mut closure" =
    run_it {|
      let mut f = fn x -> x in
      f 0
    |};
    [%expect {| '__7 |}]

  let%expect_test "mut closure" =
    run_it
      {|
      let mut f = fn x -> x in
      f 0;
      f = fn x -> x + 1;
      f 0
    |};
    [%expect {| '__7 |}]

  let%expect_test "disjoint mut" =
    run_it
      {|
      let mut f = fn x -> x in
      f 0;
      f = 7;
      f 0
    |};
    [%expect {| '__7 |}]

  let%expect_test "value restriction" =
    run_it
      {|
      let mut c = fn x -> x in
      c = (fn x -> x + 1);
      c true
      |};
    [%expect {| () |}]

  let%expect_test "forever" =
    run_it {| for do () end |};
    [%expect {| () |}]

  let%expect_test "counter loop" =
    run_it
      {| 
      let mut i = 0 in
      for do
        i = i + 1
      end;
      i
    |};
    [%expect {| int <: '__0 <: int |}]

  let%expect_test "ref counter loop" =
    run_it
      {| 
      let ref i = 0 in
      for do
        i := (!i + 1)
      end;
      !i
    |};
    [%expect {| int <: '__3 |}]

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

  let%expect_test "reference can be captured as readonly" =
    run_it {|
      let ref xs = 0 in
      xs |};
    [%expect {| ref[int <: '__0, _] @ 0 |}]

  let%expect_test "reference cannot be captured as readwrite" =
    run_it {|
      let ref xs = 0 in
      &mut xs |};
    [%expect
      {|
     Type error
       expected: ref[int <: '__0, int <: '__1]
       actual: ref[int <: '__0, int <: '__1] @ 0
       span: stdin:3:6:3:13 |}]

  let%expect_test "readonly reference cannot be written" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = xs[..] in
      ys[1] = 9|};
    [%expect
      {|
      Type error
        expected: vector[_, int] @ 0
        actual: vector[int | int <: '__1 @ 1, _] @ 0
        span: stdin:4:6:4:15 |}]

  let%expect_test "readwrite reference can be read" =
    run_it {|
      let ref mut xs = 0 in
      let ys = &mut xs in
      !ys |};
    [%expect {| int <: '__2 |}]

  let%expect_test "readonly reference can be read" =
    run_it {|
      let ref mut xs = 0 in
      let ys = xs in
      !ys |};
    [%expect {| int <: '__2 |}]

  let%expect_test "readwrite reference be written" =
    run_it
      {|
      let ref mut xs = 0 in
      let ys = &mut xs in
      ys := 10 |};
    [%expect {| () |}]

  let%expect_test "captured readonly reference  cannot be written" =
    run_it
      {|
      let ref mut xs = 0 in
      let ys = xs in
      let f x -> ys := x in
      f () |};
    [%expect
      {|    
      Type error
        expected: ref[_, '__2 @ 1]
        actual: ref[int <: '__0, _]
        span: stdin:4:17:4:24 |}]

  let%expect_test "captured readwrite reference can only be read" =
    run_it
      {|
      let ref mut xs = 0 in
      let ys = &mut xs in
      let f x -> ys in
      f () |};
    [%expect {| ref[int <: '__0, _]  <: '__3 |}]

  let%expect_test "passed readonly reference cannot be written" =
    run_it
      {|
      let ref mut xs = 0 in
      let f ys -> ys := 99 in
      f xs |};
    [%expect
      {| 
      Type error
        expected: ref[_, int]
        actual: '__2 @ 1
        span: stdin:3:18:3:26|}]

  let%expect_test "passed readwrite reference cannot be written without \
                   annotations" =
    run_it
      {|
      let ref mut xs = 0 in
      let f ys -> ys := 99 in
      f &mut xs |};
    [%expect
      {| 
      Type error
        expected: ref[_, int]
        actual: '__2 @ 1
        span: stdin:3:18:3:26 |}]

  let%expect_test "passed readonly reference be read" =
    run_it
      {|x
      let ref mut xs = 0 in
      let f ys -> !ys in
      f xs |};
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "passed readonly reference cannot be made writable" =
    run_it
      {|
      let ref mut xs = 0 in
      let f ys -> 
        let xs = &mut ys in
        xs := 2 in
      f xs |};
    [%expect
      {|
        Type error
          expected: ref['__3 @ 2, '__4 @ 2]
          actual: '__2 @ 1
          span: stdin:4:17:4:24 |}]
end
