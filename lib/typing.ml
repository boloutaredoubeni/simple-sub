open Core
open Text
open Tast

exception Unbound_variable of { value : Symbol.t; span : Span.t }
exception Missing_record_field of { value : Symbol.t }
exception Missing_case of { value : Symbol.t }
exception Missing_tuple_index of { value : int }
exception Constrain_error of { lhs : Simple_type.t; rhs : Simple_type.t }

exception
  Type_error of {
    expected : Simple_type.t;
    actual : Simple_type.t;
    span : Span.t;
  }

exception
  Slice_of_list_error of { value : Symbol.t; span : Span.t; message : string }

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
                val mutable freshened = Map.empty (module Symbol)
                method get (v : Variable.t) = Map.find freshened v.name

                method set (v : Variable.t) t =
                  freshened <- Map.add_exn freshened ~key:v.name ~data:t
              end
            in

            let rec freshen ty =
              let open Level in
              match ty with
              | t when Simple_type.level t <= limit -> t
              | Scontinuation { argument; scope } ->
                  Scontinuation { argument = freshen argument; scope }
              | Smutable { read; write; scope } ->
                  Smutable { read = freshen read; write = freshen write; scope }
              | Svar_type { state } when Option.is_some (freshened#get state) ->
                  Option.value_exn (freshened#get state)
              | Svar_type { state } ->
                  let fresh = Fresh_var.f lvl in
                  freshened#set state fresh;
                  let fresh =
                    match fresh with
                    | Svar_type { state } -> state
                    | _ -> assert false
                  in
                  fresh.lower_bounds <-
                    List.rev
                      (List.map (List.rev state.lower_bounds) ~f:(fun t ->
                           freshen t));
                  fresh.upper_bounds <-
                    List.rev
                      (List.map (List.rev state.upper_bounds) ~f:(fun t ->
                           freshen t));
                  Variable.to_simple_type fresh
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
              | Slist_type { read; write; scope } ->
                  let read = Option.map read ~f:freshen in
                  let write = Option.map write ~f:freshen in
                  Slist_type { read; write; scope }
              | Srecord { fields } ->
                  Srecord
                    {
                      fields = List.map fields ~f:(fun (f, t) -> (f, freshen t));
                    }
              | Scases { cases } ->
                  Scases
                    { cases = List.map cases ~f:(fun (f, t) -> (f, freshen t)) }
              | Sint_type -> Sint_type
              | Sbool_type -> Sbool_type
              | Sfloat_type -> Sfloat_type
              | Sunit_type -> Sunit_type
              | Schar_type -> Schar_type
              | Sstring_type -> Sstring_type
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
        val mutable map = Map.empty (module Symbol)
        method add ~key ~data = map <- Map.add_exn map ~key ~data
        method contains (key : Symbol.t) = Map.mem map key
        method find_exn (key : Symbol.t) = Map.find_exn map key

        method find_or_else key ~else' =
          match Map.find map key with Some v -> v | None -> else' ()
      end

    let rec extrude self t =
      let open Simple_type in
      let polarity = Polar.Type.polarity t in
      let create_type t = Polar.Type.create t polarity in
      let open Level in
      match Polar.Type.type_of t with
      | _ when Polar.Type.level t <= Self.level self -> Polar.Type.type_of t
      | Sint_type -> Sint_type
      | Sfloat_type -> Sfloat_type
      | Sbool_type -> Sbool_type
      | Sunit_type -> Sunit_type
      | Schar_type -> Schar_type
      | Sstring_type -> Sstring_type
      | Scontinuation { argument; scope } ->
          Scontinuation
            { argument = extrude self (create_type argument); scope }
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
      | Slist_type { read; write; scope } ->
          let read =
            Option.map read ~f:(fun read -> extrude self (create_type read))
          in
          let write =
            Option.map write ~f:(fun write ->
                extrude self (Polar.Type.create write (Polar.not polarity)))
          in
          Slist_type { read; write; scope }
      | Srecord { fields } ->
          Srecord
            {
              fields =
                List.map fields ~f:(fun (f, t) ->
                    (f, extrude self (create_type t)));
            }
      | Scases { cases } ->
          Scases
            {
              cases =
                List.map cases ~f:(fun (f, t) ->
                    (f, extrude self (create_type t)));
            }
      | Svar_type { state } when c#contains state.name ->
          Svar_type { state = c#find_exn state.name }
      | Svar_type { state } ->
          let nvs =
            Variable.create ~level:(Self.level self)
              ~fresher:(module S.Fresh_sym)
          in
          let nvs_state = nvs |> Variable.of_simple_type |> Option.value_exn in
          c#add ~key:state.name ~data:nvs_state;
          if Polar.bool polarity then (
            state.upper_bounds <-
              Svar_type { state = nvs_state } :: state.upper_bounds;
            nvs_state.lower_bounds <-
              List.map state.lower_bounds ~f:(fun ty ->
                  extrude self (Polar.Type.create ty polarity)))
          else
            state.lower_bounds <-
              Svar_type { state = nvs_state } :: state.lower_bounds;

          nvs_state.upper_bounds <-
            List.map state.upper_bounds ~f:(fun ty ->
                extrude self (Polar.Type.create ty polarity));
          nvs

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
        | Sstring_type, Sstring_type -> ()
        | ( Scontinuation { argument = rhs; _ },
            Scontinuation { argument = lhs; _ } ) ->
            constrain (Constraint { lhs; rhs })
        | Scontinuation _, _ -> failwith "continuations in lhs"
        | _, Scontinuation _ -> failwith "continuations in rhs"
        | Schar_type, Schar_type -> ()
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
        | ( Slist_type { read = lhs_read; write = lhs_write; _ },
            Slist_type { read = rhs_read; write = rhs_write; _ } ) -> (
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
        | Scases { cases = lhs_cases }, Scases { cases = rhs_cases } ->
            List.iter lhs_cases ~f:(fun (lhs_case, lhs_type) ->
                match
                  List.Assoc.find rhs_cases lhs_case ~equal:Symbol.equal
                with
                | Some rhs_type ->
                    constrain (Constraint { lhs = rhs_type; rhs = lhs_type })
                | None -> raise (Missing_case { value = lhs_case }))
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
            lhs_state.upper_bounds <- rhs :: lhs_state.upper_bounds;
            List.iter lhs_state.lower_bounds ~f:(fun lhs ->
                constrain (Constraint { lhs; rhs }))
        | lhs, Svar_type { state = rhs_state }
          when Simple_type.level lhs <= Variable.level rhs_state ->
            rhs_state.lower_bounds <- lhs :: rhs_state.lower_bounds;
            List.iter rhs_state.upper_bounds ~f:(fun rhs ->
                constrain (Constraint { lhs; rhs }))
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
  val map : Syntax.t -> Tast.t Or_error.t
  val map_ast : self -> Syntax.t -> Tast.t
  val map_closure : self -> Syntax.Closure.t -> Closure.t
  val map_iterate : self -> Syntax.iterate -> self * Tast.Iterate.t
  val map_cases : self -> Syntax.alt -> Simple_type.t * Tast.Alt.t
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
      | Uchar { value; span } -> Tchar { value; span }
      | Ustring { value; span } -> Tstring { value; span }
      | Uneg { value; span } ->
          let value = map_ast self value in
          constrain (Tast.T.type_of value) Sint_type;
          Tprimop { op = Tint_neg; args = [ value ]; span; type' = Sint_type }
      | Uop { op = SAdd; left; right; span } ->
          let left = map_ast self left in
          let right = map_ast self right in
          constrain (Tast.T.type_of left) Sstring_type;
          constrain (Tast.T.type_of right) Sstring_type;
          Tprimop
            {
              op = Tstr_concat;
              args = [ left; right ];
              span;
              type' = Sstring_type;
            }
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
            | Eq, lhs, Sint_type ->
                constrain lhs Sint_type;
                Tint_eq
            | op, lhs, rhs ->
                failwith
                  (Printf.sprintf "invalid relop: %s %s %s"
                     (Simple_type.to_string
                        ~visited:(Set.empty (module Symbol))
                        lhs)
                     (Op.to_string op)
                     (Simple_type.to_string
                        ~visited:(Set.empty (module Symbol))
                        rhs))
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
      | Ulist { values; span; mutability = Mutability.Immutable } ->
          let values = List.map values ~f:(map_ast self) in

          let read = S.Fresh_var.f (Self.level self) in
          List.iter values ~f:(fun value ->
              constrain (Tast.T.type_of value) read);
          let type' =
            Slist_type
              { read = Some read; write = None; scope = Self.scope self }
          in
          Tlist { values; span; type' }
      | Ulist { values; span; mutability = Mutability.Mutable } ->
          let values = List.map values ~f:(map_ast self) in
          let write = S.Fresh_var.f (Self.level self) in
          let read = S.Fresh_var.f (Self.level self) in
          List.iter values ~f:(fun value ->
              constrain (Tast.T.type_of value) read);
          let type' =
            Slist_type
              { read = Some read; write = Some write; scope = Self.scope self }
          in
          Tlist { values; span; type' }
      | Ulist { values; span; mutability = Mutability.Reference } ->
          let values = List.map values ~f:(map_ast self) in
          let write = S.Fresh_var.f (Self.level self) in
          let read = S.Fresh_var.f (Self.level self) in
          List.iter values ~f:(fun value ->
              constrain (Tast.T.type_of value) read);
          let type' =
            Slist_type
              { read = Some read; write = Some write; scope = Scope.Global }
          in
          Tlist { values; span; type' }
      | Ulist { mutability = Mutability.MutableReference; _ } -> assert false
      | Uslice
          { value; readability = Readability.Readonly; span; start; finish }
        when Self.contains self value ->
          let type' =
            let scheme = Self.find_exn self value in
            match TypeScheme.instantiate scheme (Self.level self) with
            | Slist_type
                { read = Some read; write = Some _; scope = Scope.Global } ->
                Slist_type
                  { read = Some read; write = None; scope = Scope.default }
            | Slist_type { read = Some _; write = None; _ } ->
                raise
                  (Slice_of_list_error
                     { value; span; message = "list is already readonly" })
            | Slist_type { read = Some _; write = Some _; _ } ->
                raise
                  (Slice_of_list_error
                     {
                       value;
                       span;
                       message =
                         "mutable lists cannot explicitly be made readonly";
                     })
            | ty ->
                let level = Self.level self in
                let res = S.Fresh_var.f level in
                let expected =
                  Sreference
                    { read = Some res; write = Some res; scope = Scope.Global }
                in
                raise (Type_error { expected; actual = ty; span })
          in
          let start = Option.map start ~f:(map_ast self) in
          Option.iter start ~f:(fun start ->
              constrain (Tast.T.type_of start) Sint_type);
          let finish = Option.map finish ~f:(map_ast self) in
          Option.iter finish ~f:(fun finish ->
              constrain (Tast.T.type_of finish) Sint_type);
          Tslice { name = value; span; start; finish; type' }
      | Uslice
          { value; readability = Readability.ReadWrite; span; start; finish }
        when Self.contains self value ->
          let type' =
            let scheme = Self.find_exn self value in
            match TypeScheme.instantiate scheme (Self.level self) with
            | Slist_type
                { read = Some read; write = Some write; scope = Scope.Global }
              ->
                Slist_type
                  { read = Some read; write = Some write; scope = Scope.Global }
            | Slist_type { read = Some _; write = None; _ } ->
                raise
                  (Slice_of_list_error
                     { value; span; message = "list is readonly" })
            | Slist_type { read = Some _; write = Some _; _ } ->
                raise
                  (Slice_of_list_error
                     {
                       value;
                       span;
                       message = "mutable lists are already readwrite locally";
                     })
            | ty ->
                let level = Self.level self in
                let res = S.Fresh_var.f level in
                let expected =
                  Slist_type
                    { read = Some res; write = Some res; scope = Scope.Global }
                in
                raise (Type_error { expected; actual = ty; span })
          in
          let start = Option.map start ~f:(map_ast self) in
          Option.iter start ~f:(fun start ->
              constrain (Tast.T.type_of start) Sint_type);
          let finish = Option.map finish ~f:(map_ast self) in
          Option.iter finish ~f:(fun finish ->
              constrain (Tast.T.type_of finish) Sint_type);
          Tslice { name = value; span; start; finish; type' }
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
            (Simple_type.Slist_type
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
                let res = S.Fresh_var.f level in
                let scope = Self.scope self in
                let expected = Smutable { read = res; write = res; scope } in
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
                let expected =
                  Sreference
                    {
                      read = Some read;
                      write = Some read;
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
            | Slist_type { scope = Scope.Global; write = Some write; _ } ->
                write
            | Slist_type { scope; write = Some write; _ }
              when Scope.(scope = Self.scope self) ->
                write
            | ty ->
                raise
                  (Type_error
                     {
                       expected =
                         Simple_type.Slist_type
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
      | Ucase { case; value; span } ->
          let value = map_ast self value in
          Tcase { case; value; span }
      | Umatch { value; cases; span } ->
          let value = map_ast self value in
          let type', cases = map_cases self cases in
          let cases_types = Tast.Alt.case_types cases in
          constrain (Tast.T.type_of value) (Scases { cases = cases_types });
          Tmatch { value; cases; type'; span }
      | Urecord { proto = None; fields; span } ->
          let res = S.Fresh_var.f (Self.level self) in
          let proto = S.Fresh_sym.f () in
          let fields =
            List.map fields ~f:(fun (field, e) -> (field, map_ast self e))
          in
          let field_types =
            List.map fields ~f:(fun (field, e) -> (field, Tast.T.type_of e))
          in
          let type' = Srecord { fields = field_types } in
          Trecord { proto = (proto, res); fields; span; type' }
      | Urecord { proto = Some proto; fields; span }
        when Self.contains self proto ->
          let scheme = Self.find_exn self proto in
          let proto_fields, res =
            match TypeScheme.instantiate scheme (Self.level self) with
            | Srecord { fields } as res -> (fields, res)
            | actual ->
                let expected = Srecord { fields = [] } in
                raise (Type_error { expected; actual; span })
          in
          let fields =
            List.map fields ~f:(fun (field, e) -> (field, map_ast self e))
          in
          let field_types =
            List.map fields ~f:(fun (field, e) -> (field, Tast.T.type_of e))
          in
          let field_types =
            List.merge
              ~compare:(fun (incoming, _) (proto, _) ->
                Symbol.compare incoming proto)
              field_types proto_fields
          in
          let type' = Srecord { fields = field_types } in
          Trecord { proto = (proto, res); fields; span; type' }
      | Urecord { proto = Some proto; span; _ } ->
          raise (Unbound_variable { value = proto; span })
      | Uselect { value; field; span } ->
          let value = map_ast self value in
          let res = S.Fresh_var.f (Self.level self) in
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
          constrain type' read;
          let type' =
            Simple_type.Sreference
              { read = Some read; write = Some read; scope = Self.scope self }
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
          constrain type' read;
          let type' =
            Simple_type.Sreference
              { read = Some read; write = Some read; scope = Scope.global }
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
          constrain type' read;
          let type' =
            Simple_type.Smutable { read; write = read; scope = Self.scope self }
          in
          let scheme = TypeScheme.of_simple_type type' in
          let app = map_ast (Self.add self ~key:binding ~data:scheme) app in
          let type' = TypeScheme.instantiate scheme level in
          Tlet { binding = (binding, type'); value; app; span }
      | Ulet { binding; value; app; span; mutability = Mutability.Immutable } ->
          let value = map_ast (Self.level_up self) value in
          let level = Self.level self in
          let scheme =
            match Tast.T.type_of value with
            | Sfunction_type _ as type' ->
                TypeScheme.of_polymorphic_type
                  (PolymorhicType.create level type')
            | type' -> TypeScheme.of_simple_type type'
          in
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
            let self = Self.level_up self in
            let type' = TypeScheme.of_simple_type expr in
            let self = Self.add self ~key:name ~data:type' in
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
      | Uresume { continuation; value; span }
        when Self.contains self continuation ->
          let scheme = Self.find_exn self continuation in
          let continuation_type =
            TypeScheme.instantiate scheme (Self.level self)
          in
          let value = map_ast self value in
          constrain continuation_type
            (Simple_type.Scontinuation
               { argument = Tast.T.type_of value; scope = Self.scope self });
          Tresume { continuation; value; span }
      | Uresume { continuation; span; _ } ->
          raise (Unbound_variable { value = continuation; span })

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
            let scheme = TypeScheme.of_simple_type Sint_type in
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

    and map_closure self = function
      | Syntax.Uclosure { parameter; value; span } ->
          let self = Self.incr_scope self in
          let type' = S.Fresh_var.f (Self.level self) in

          let self =
            let type' = TypeScheme.of_simple_type type' in
            Self.add self ~key:parameter ~data:type'
          in
          Tclosure
            { parameter = (parameter, type'); value = map_ast self value; span }
      | Syntax.Ususpend { continuation; body; span } ->
          let self = Self.incr_scope self in
          let argument = S.Fresh_var.f (Self.level self) in
          let scope = Self.scope self in
          let cont = Simple_type.Scontinuation { argument; scope } in
          let self =
            let type' = TypeScheme.of_simple_type cont in
            Self.add self ~key:continuation ~data:type'
          in
          Tsuspend
            {
              continuation = (continuation, argument);
              value = map_ast self body;
              span;
            }

    and map_cases self cases : Simple_type.t * Tast.Alt.t =
      let found = ref [] in
      let type' = S.Fresh_var.f (Self.level self) in
      let rec loop self =
        let open Syntax in
        let open Tast.Alt in
        function
        | Uno_match -> Tno_match
        | Ualt { name = None; tag; expr; span; rest }
          when not (List.mem !found tag ~equal:Symbol.equal) ->
            found := tag :: !found;
            let name = S.Fresh_sym.f () in
            let ty = S.Fresh_var.f (Self.level self) in
            let scheme =
              TypeScheme.of_polymorphic_type
                (PolymorhicType.create (Self.level self) ty)
            in
            let rhs_value =
              map_ast
                (Self.add (Self.level_up self) ~key:name ~data:scheme)
                expr
            in
            let rhs = Tast.T.type_of rhs_value in
            constrain rhs type';

            let rest = loop self rest in
            Talt { name = (name, ty); tag; expr = rhs_value; span; rest }
        | Ualt { name = Some name; tag; expr; span; rest }
          when not (List.mem !found tag ~equal:Symbol.equal) ->
            found := tag :: !found;
            let ty = S.Fresh_var.f (Self.level self) in
            let scheme =
              TypeScheme.of_polymorphic_type
                (PolymorhicType.create (Self.level self) ty)
            in
            let rhs_value =
              map_ast
                (Self.add (Self.level_up self) ~key:name ~data:scheme)
                expr
            in
            let rhs = Tast.T.type_of rhs_value in
            constrain rhs type';

            let rest = loop self rest in
            Talt { name = (name, ty); tag; expr = rhs_value; span; rest }
        | Ualt { tag; _ } when List.mem !found tag ~equal:Symbol.equal ->
            failwith "dupe tag"
        | Ualt _ -> assert false
      in
      (type', loop self cases)

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
       t |> Tast.T.type_of
       |> Simple_type.to_string ~visited:(Set.empty (module Symbol))
       |> Ok)
      |> Or_error.ok_exn |> print_endline
    with
    | () -> ()
    | exception Type_error { expected; actual; span } ->
        print_endline
          (Printf.sprintf "Type error\n  expected: %s\n  actual: %s\n  span: %s"
             (Simple_type.to_string
                ~visited:(Set.empty (module Symbol))
                expected)
             (Simple_type.to_string ~visited:(Set.empty (module Symbol)) actual)
             (Span.to_string span))
    | exception Constrain_error { lhs; rhs } ->
        print_endline
          (Printf.sprintf "Constrain error\n  lhs: %s\n  rhs: %s"
             (Simple_type.to_string ~visited:(Set.empty (module Symbol)) lhs)
             (Simple_type.to_string ~visited:(Set.empty (module Symbol)) rhs))
    | exception Missing_case { value } ->
        print_endline
          (Printf.sprintf "Missing case\n  value: %s" (Symbol.to_string value))
    | exception Missing_record_field { value } ->
        print_endline
          (Printf.sprintf "Missing record field\n  field: %s"
             (Symbol.to_string value))
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
    [%expect {| int <: '__2 |}]

  let%expect_test "type lambda" =
    run_it {| fn x -> 10 end |};
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
    [%expect {| int <: '__5 -> '__4 <: __4 <: '__3 <: int -> '__4 <: __4 |}]

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
    [%expect {|
      Constrain error
        lhs: float
        rhs: int |}]

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

  let%expect_test "empty list" =
    run_it "[||]";
    [%expect {| list['__0, _] @ 0 |}]

  let%expect_test "list" =
    run_it "[| 1, 2 |]";
    [%expect {| list[int | int <: '__0, _] @ 0 |}]

  let%expect_test "union list" =
    run_it "[| 1, true |]";
    [%expect {| list[bool | int <: '__0, _] @ 0 |}]

  let%expect_test "list subscript" =
    run_it "let xs = [| 1, 2 |] in xs[0]";
    [%expect {| int <: '__1 |}]

  let%expect_test "union list subscript" =
    run_it "let xs = [| true, 2 |] in xs[0]";
    [%expect {| bool | int <: '__1 |}]

  let%expect_test "let mut" =
    run_it "let mut x = 1 in x";
    [%expect {| int <: '__0 |}]

  let%expect_test "mut update" =
    run_it "x = 1";
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "list update" =
    run_it "xs[0] = 1";
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "readonly list" =
    run_it {|
      let xs = [| 0, 1|] in
      xs[0] = 1 |};
    [%expect
      {|
     Type error
       expected: list[_, int] @ 0
       actual: list[int | int <: '__0 @ 1, _] @ 0
       span: stdin:3:6:3:15 |}]

  let%expect_test "local readwrite list" =
    run_it {|
       let xs = mut [| 0, 1|] in
       xs[0] = 1 |};
    [%expect {| () |}]

  let%expect_test "readwrite list, no passing" =
    run_it {|
       let xs = ref [| 0, 1|] in
       xs[0] = 1 |};
    [%expect {| () |}]

  let%expect_test "readwrite list, capture readonly" =
    run_it
      {|
       let xs = mut [| 0, 1|] in
       let f x -> xs[0] = x in
       xs[0] = 1;
       f 1|};
    [%expect
      {|
     Type error
       expected: list[_, '__2 @ 1] @ 1
       actual: list[int | int <: '__1 @ 1, '__0 @ 1] @ 0
       span: stdin:3:18:3:27 |}]

  let%expect_test "readwrite list, capture readwrite" =
    run_it
      {|
       let xs = ref [| 0, 1|] in
       let f x -> xs[0] = x in
       xs[0] = 1;
       f 1|};
    [%expect {| () <: '__3 |}]

  let%expect_test "readonly list" =
    run_it "xs[..]";
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "readwrite list" =
    run_it "&mut xs[..]";
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "immutable lists are already readonly" =
    run_it {|
      let xs = [| 0, 1|] in
      xs[..] |};
    [%expect
      {| ("Fx__Typing.Slice_of_list_error(_, _, \"list is already readonly\")") |}]

  let%expect_test "immutable lists cannot be made writeable" =
    run_it {|
       let xs = [| 0, 1|] in
       &mut xs[..] |};
    [%expect
      {| ("Fx__Typing.Slice_of_list_error(_, _, \"list is readonly\")") |}]

  let%expect_test "mutable lists can only be captured as readonly" =
    run_it {|
      let xs = mut [| 0, 1|] in
      xs[..] |};
    [%expect
      {| ("Fx__Typing.Slice_of_list_error(_, _, \"mutable lists cannot explicitly be made readonly\")") |}]

  let%expect_test "mutable lists can only be captured as readonly" =
    run_it {|
      let xs = mut [| 0, 1|] in
      &mut xs[..] |};
    [%expect
      {| ("Fx__Typing.Slice_of_list_error(_, _, \"mutable lists are already readwrite locally\")") |}]

  let%expect_test "reference lists can be captured as readonly" =
    run_it {|
      let xs = ref [| 0, 1|] in
      xs[..] |};
    [%expect {| list[int | int <: '__1 @ 1, _] @ 0 |}]

  let%expect_test "reference lists can be captured as readonly" =
    run_it {|
      let xs = ref [| 0, 1|] in
      &mut xs[..] |};
    [%expect {| list[int | int <: '__1 @ 1, '__0 @ 1] |}]

  let%expect_test "readonly lists cannot be written" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = xs[..] in
      ys[1] = 9|};
    [%expect
      {|
     Type error
       expected: list[_, int] @ 0
       actual: list[int | int <: '__1 @ 1, _] @ 0
       span: stdin:4:6:4:15 |}]

  let%expect_test "readwrite lists can be read" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = &mut xs[..] in
      ys[1] |};
    [%expect {| int <: '__2 |}]

  let%expect_test "readonly lists can be read" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = xs[..] in
      ys[1] |};
    [%expect {| int <: '__2 |}]

  let%expect_test "readwrite lists be written" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = &mut xs[..] in
      ys[1] = 10 |};
    [%expect {| () |}]

  let%expect_test "captured readonly lists cannot be written" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = xs[..] in
      let f x -> ys[1] = x in
      f () |};
    [%expect
      {|
       Type error
         expected: list[_, '__2 @ 1] @ 1
         actual: list[int | int <: '__1 @ 1, _] @ 0
         span: stdin:4:17:4:26 |}]

  let%expect_test "captured readwrite lists can only be read" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let ys = &mut xs[..] in
      let f x -> ys[1] in
      f () |};
    [%expect {| int <: '__4 |}]

  let%expect_test "passed readonly lists cannot be written" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let f ys -> ys[1] = 99 in
      f xs[..] |};
    [%expect
      {|
     Type error
       expected: list[_, int] @ 1
       actual: '__2 @ 1
       span: stdin:3:18:3:28 |}]

  let%expect_test "passed writeonly lists cannot be written without annotations"
      =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let f ys -> ys[1] = 99 in
      f &mut xs[..] |};
    [%expect
      {|
     Type error
       expected: list[_, int] @ 1
       actual: '__2 @ 1
       span: stdin:3:18:3:28 |}]

  let%expect_test "passed readonly lists be read" =
    run_it
      {|
      let xs = ref [| 0, 1|] in
      let f ys -> ys[1] in
      f xs[..] |};
    [%expect {| int <: '__4 |}]

  let%expect_test "passed readonly lists cannot be made writable" =
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
       expected: list['__3 @ 2, '__3 @ 2]
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
    [%expect {| int <: '__1 |}]

  let%expect_test "let ref update union" =
    run_it {|
      let ref a = 0 in
      a := 1.0;
      a
    |};
    [%expect {| ref[float | int <: '__0, _] @ 0 |}]

  let%expect_test "let ref deref union" =
    run_it {|
      let ref a = 0 in
      a := 1.0;
      !a
    |};
    [%expect {| int | float <: '__1 |}]

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
       expected: mut[int <: '__0, int <: '__0] @ 1
       actual: mut[int <: '__0, int <: '__0] @ 0
       span: stdin:3:17:3:22 |}]

  let%expect_test "declare mut closure" =
    run_it {|
      let mut f = fn x -> x end in
      f 0
    |};
    [%expect {| int <: '__3 |}]

  let%expect_test "mut closure" =
    run_it
      {|
      let mut f = fn x -> x end in
      f 0;
      (f = (fn x -> x + 1 end)) ;
      f 0
    |};
    [%expect {| int <: '__3 |}]

  let%expect_test "disjoint mut" =
    run_it
      {|
      let mut f = fn x -> x end in
      (f 0);
      (f = 7);
      f 0
    |};
    [%expect
      {|
      Constrain error
        lhs: int
        rhs: int -> int <: '__3 |}]

  let%expect_test "value restriction" =
    run_it
      {|
      let mut c = fn x -> x end in
      (c = (fn x -> x + 1 end));
      c true
      |};
    [%expect
      {|
        Constrain error
          lhs: bool
          rhs: int |}]

  let%expect_test "value restriction ref" =
    run_it
      {|
      let ref c = fn x -> x end in
      (c := (fn x -> x + 1 end));
      !c true
      |};
    [%expect {|
     Constrain error
       lhs: bool
       rhs: int |}]

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
    [%expect {| int | int <: '__0 <: int |}]

  let%expect_test "ref counter loop" =
    run_it
      {| 
      let ref i = 0 in
      for do
        i := (!i + 1)
      end;
      !i
    |};
    [%expect {| int <: '__2 |}]

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
       expected: ref[int <: '__0, int <: '__0]
       actual: ref[int <: '__0, int <: '__0] @ 0
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
        expected: list[_, int] @ 0
        actual: list[int | int <: '__1 @ 1, _] @ 0
        span: stdin:4:6:4:15 |}]

  let%expect_test "readwrite reference can be read" =
    run_it {|
      let ref mut xs = 0 in
      let ys = &mut xs in
      !ys |};
    [%expect {| int <: '__1 |}]

  let%expect_test "readonly reference can be read" =
    run_it {|
      let ref mut xs = 0 in
      let ys = xs in
      !ys |};
    [%expect {| int <: '__1 |}]

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
        expected: ref[_, '__1 @ 1]
        actual: ref[int <: '__0, _]
        span: stdin:4:17:4:24 |}]

  let%expect_test "captured readwrite reference can only be read" =
    run_it
      {|
      let ref mut xs = 0 in
      let ys = &mut xs in
      let f x -> ys in
      f () |};
    [%expect {| ref[int <: '__0, _]  <: '__2 |}]

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
        actual: '__1 @ 1
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
        actual: '__1 @ 1
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
          expected: ref['__2 @ 2, '__2 @ 2]
          actual: '__1 @ 1
          span: stdin:4:17:4:24 |}]

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
    [%expect {| {a: int, b: int, c: int} | {a: int, b: int} <: '__0 |}]

  let%expect_test "record contraction" =
    run_it
      {|
        let r = if true then
          { a = 0, b = 1 }
        else
          { a = 0, b = 1, c = 2 } in
        r.c
    |};
    [%expect {|
      Missing record field
        field: c |}]

  let%expect_test "record union" =
    run_it
      {|
       if true then
          { a = 0, b = 1 }
        else
          { a = 0, b = '1' } 

    |};
    [%expect {| {a: int, b: char} | {a: int, b: int} <: '__0 |}]

  let%expect_test "record union fields" =
    run_it
      {|
        let r = if true then
          { a = 0, b = 1 }
        else
          { a = 0, b = '1' } in
        r.b
    |};
    [%expect {| int | char <: '__5 |}]

  let%expect_test "record fun" =
    run_it {|
      let f r -> r.a in
      f { a = 0, b = 1 }
   |};
    [%expect {| int <: '__2 |}]

  let%expect_test "record fun union" =
    run_it
      {|
      let f r -> r.a in
      (f { a = 0, b = 1 }) + (f { a = 0, b = '1' })
   |};
    [%expect {| int |}]

  let%expect_test "cons list" =
    run_it
      {|
         def produce arg = { head = arg, tail = (produce (arg + 1)) } in
         produce
       |};
    [%expect
      {| int <: '__6 <: int -> {head: int <: '__6 <: int, tail: {head: int <: '__6 <: int, tail: __7} <: '__7} <: '__5 <: int -> {head: int <: '__6 <: int, tail: __7} <: '__7 |}]

  let%expect_test "cons list as record" =
    run_it
      {|
         def consume strm = (strm.head + consume strm.tail) in
         consume
       |};
    [%expect
      {| '__6 <: {tail: '__8 <: __6} | {head: '__7 <: int} -> int <: '__5 <: '__8 <: '__6 <: {tail: __8} | {head: '__7 <: int} -> int <: '__9 <: int |}]

  let%expect_test "cons list as record" =
    run_it
      {|
      def produce arg = { head = arg, tail = (produce (arg + 1)) } in 
      def consume strm = (strm.head + consume strm.tail) in
    
      let codata = produce 42 in
      consume codata
    |};
    [%expect {| int <: '__14 |}]

  let%expect_test "record extension" =
    run_it
      {|
          let r = { a = 0, b = 1 } in
          { r with c = 2 }
    |};
    [%expect {| {a: int, b: int, c: int} |}]

  let%expect_test "record extend let mut" =
    run_it
      {|
      let mut r = { a = 0, b = 1 } in
      (r = { r with c = 2 });
      r
    |};
    [%expect
      {|
        Type error
          expected: {}
          actual: mut[{a: int, b: int} <: '__2, {a: int, b: int} <: '__2] @ 0
          span: stdin:3:11:3:27 |}]

  let%expect_test "record extend let ref" =
    run_it
      {|
      let ref r = { a = 0, b = 1 } in
      let s = !r in
      (r := { s with c = 2 });
      !r
    |};
    [%expect
      {|
      Type error
        expected: {}
        actual: {a: int, b: int} <: '__2 <: '__3 @ 1
        span: stdin:4:12:4:28 |}]

  let%expect_test "nested function" =
    run_it
      {|
      let f = 
        let x = 0 in
        fn y -> x + y end in
      f
    |};
    [%expect {| '__1 <: int -> int |}]

  let%expect_test "curried function" =
    run_it {|
      let f x -> fn y -> x + y end in
      f
    |};
    [%expect {| '__3 <: int -> '__2 <: int -> int |}]

  let%expect_test "iife" =
    run_it {| (fn x -> x end) 0 |};
    [%expect {| int <: '__0 |}]

  let%expect_test "cases" =
    run_it {| Some 1 |};
    [%expect {| [case Some int] |}]

  let%expect_test "unit case" =
    run_it {| None |};
    [%expect {| [case None ()] |}]

  let%expect_test "can only match cases" =
    run_it {| 
        match 0 
          case Ok x -> ()
        end
    |};
    [%expect
      {|
         Constrain error
           lhs: int
           rhs: [case Ok '__1] |}]

  let%expect_test "cases must be handled" =
    run_it {| 
        match B
          case A x -> ()
        end
    |};
    [%expect {|
        Missing case
          value: B
         |}]

  let%expect_test "disjoint match" =
    run_it
      {| 
        match B
          case A x -> x.foo,
          case B -> false
        end
    |};
    [%expect {|  bool <: '__0 |}]

  let%expect_test "suspend block" =
    run_it {|
        suspend k =>() end
    |};
    [%expect {| '__0 |}]

  let%expect_test "suspend function" =
    run_it {|
      let f x k => () in
      f
    |};
    [%expect {| '__3 -> '__2 |}]

  let%expect_test "suspend rec" =
    run_it {|
      def f x k => () in
      f
    |};
    [%expect {| '__5 -> '__4 <: '__3 |}]

  let%expect_test "suspend lambda" =
    run_it {|
      fn x k => () end
    |};
    [%expect {| '__0 -> '__1 |}]

  let%expect_test "resume value" =
    run_it {| resume k 1 |};
    [%expect {| ("Fx__Typing.Unbound_variable(_, _)") |}]

  let%expect_test "resume in suspension" =
    run_it {| suspend k => resume k 1 end|};
    [%expect {| int <: '__0 |}]

  let%expect_test "resume by if" =
    run_it {| suspend k => if true then resume k 1 end end|};
    [%expect {| int <: '__0 |}]

  let%expect_test "resume by match" =
    run_it
      {| 
      suspend k => 
        match Some 0
          case Some v -> resume k 0,
          case None -> ()
        end
      end
    |};
    [%expect {|   int <: '__0 |}]

  let%expect_test "non tail resumption" =
    run_it
      {|
      suspend k => 
        let x = resume k 1 in
        x
      end
    |};
    [%expect {| int <: '__0 |}]

  let%expect_test "multiple resumptions" =
    run_it
      {|
      suspend k => 
        (resume k 1);
        resume k 2
      end
    |};
    [%expect {| int | int <: '__0 |}]

  let%expect_test "suspended function" =
    run_it {|
      let f x k => resume k 1 in
      f
    |};
    [%expect {| '__3 -> int <: '__2 |}]

  let%expect_test "suspended rec" =
    run_it {|
      def f x k => resume k 1 in
      f
    |};
    [%expect {| '__5 -> int <: '__4 <: '__3 |}]

  let%expect_test "suspended lambda" =
    run_it {|
      fn x k => resume k 1 end
    |};
    [%expect {| '__0 -> int <: '__1 |}]

  let%expect_test "recursively applied function" =
    run_it
      {|
      def f x k => 
        match x 
          case Continue st -> 
            if st == 0 then
              f (Break st)
            else 
              f (Continue (st - 1)),
          case Break st -> resume k st
        end
      in f
    |};
    [%expect
      {| [case Break '__22 <: '__21 <: '__20 <: __20 | int -> '__21 | int | int] | [case Continue '__21 <: '__20 <: __20] <: '__19 <: [case Break '__23 <: '__22 <: '__21 <: '__20 <: __20 | int -> '__21 | int | int | '__15 <: '__18 <: () <: '__17 | '__16 <: () <: '__17| case Continue '__22 <: '__21 <: '__20 <: __20 | int -> '__21 | int | int] -> '__15 <: '__18 <: () <: '__17 | '__16 <: () <: '__17 <: '__14 <: [case Continue '__21 <: '__20 <: __20] -> '__16 <: () <: '__17 | [case Break '__22 <: '__21 <: '__20 <: __20 | int -> '__21 | int | int] -> '__18 <: () <: '__17 |}]
end
