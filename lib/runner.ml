open Core

module type T = sig
  val run : unit -> unit
end

module type S = sig
  val print_ast : bool
  val filename : string
  val print_tast : bool
  val print_lambda : bool

  module Fresh_sym : Text.FRESH_SYM
end

exception File_not_found of string

module Make (S : S) : T = struct
  module Syntax = struct
    include Syntax
    include To_syntax
  end

  module Tast = struct
    include Tast
    include T
  end

  let fresh_var = Typing.create_fresh_vars ~fresher:(module S.Fresh_sym)

  let f () =
    let open Or_error.Let_syntax in
    let%bind filename =
      match Sys_unix.file_exists S.filename with
      | `Yes -> Ok S.filename
      | `No | `Unknown -> Or_error.of_exn (File_not_found S.filename)
    in
    let%bind ast = filename |> In_channel.read_all |> Syntax.parse in
    if S.print_ast then print_endline (Sexp.to_string (Syntax.sexp_of_t ast));
    let (module Fresh_var) = fresh_var in
    let module Typing = Typing.Make (struct
      module Fresh_var = Fresh_var
      module Fresh_sym = S.Fresh_sym
    end) in
    let%bind tast = Typing.map ast in
    if S.print_tast then print_endline (Sexp.to_string (Tast.sexp_of_t tast));
    let module Coalesce = Coalesce.Make (S.Fresh_sym) in
    let%bind lambda = Coalesce.map tast in
    if S.print_lambda then
      print_endline (Sexp.to_string (Lambda.sexp_of_t lambda));
    Ok ()

  let run () =
    match f () with
    | Ok () -> ()
    | Error e -> print_endline (Error.to_string_hum e)
end
