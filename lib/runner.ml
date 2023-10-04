open Core

module type T = sig
  val run : unit -> unit
end

module type S = sig
  val print_ast : bool
  val filename : string
  val print_tast : bool
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

  module Typing = Typing.Make (struct
    open Tast

    let counter = ref 0

    let f level =
      let v = !counter in
      counter := v + 1;
      Variable.create level
  end)

  let print_ast = S.print_ast
  let filename = S.filename
  let print_tast = S.print_tast

  let f () =
    let open Or_error.Let_syntax in
    let%bind filename =
      match Sys_unix.file_exists filename with
      | `Yes -> Ok S.filename
      | `No | `Unknown -> Or_error.of_exn (File_not_found filename)
    in
    let%bind ast = filename |> In_channel.read_all |> Syntax.parse in
    if print_ast then print_endline (Sexp.to_string (Syntax.sexp_of_t ast));
    let%bind tast = Typing.visit ast in
    if print_tast then print_endline (Sexp.to_string (Tast.sexp_of_t tast));
    Ok ()

  let run () =
    match f () with
    | Ok () -> ()
    | Error e -> print_endline (Error.to_string_hum e)
end
