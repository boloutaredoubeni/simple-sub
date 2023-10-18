open Core

module type T = sig
  val run : unit -> unit
end

module type S = sig
  val print_ast : bool
  val filename : string
  val print_tast : bool
  val print_lambda : bool
end

exception File_not_found of string

module Make (S : S) : T = struct
  module Syntax = struct
    include Syntax
    include To_syntax
  end

  let f () =
    let open Or_error.Let_syntax in
    let%bind filename =
      match Sys_unix.file_exists S.filename with
      | `Yes -> Ok S.filename
      | `No | `Unknown -> Or_error.of_exn (File_not_found S.filename)
    in
    let%bind ast = filename |> In_channel.read_all |> Syntax.parse in
    if S.print_ast then print_endline (Sexp.to_string (Syntax.sexp_of_t ast));
    let (module Fresh_sym) = Text.create_fresh_sym () in
    let (module Fresh_var) =
      Typing.create_fresh_vars ~fresher:(module Fresh_sym)
    in
    let module Typing = Typing.Make (struct
      module Fresh_var = Fresh_var
      module Fresh_sym = Fresh_sym
    end) in
    let%bind tast = Typing.map ast in
    if S.print_tast then print_endline (Sexp.to_string (Tast.sexp_of_t tast));
    let module To_lambda = To_lambda.Make (Fresh_sym) in
    let%bind lambda = To_lambda.map tast in
    if S.print_lambda then
      print_endline (Sexp.to_string (Lambda.sexp_of_t lambda));
    Ok ()

  let run () =
    match f () with
    | Ok () -> ()
    | Error e -> (
        let open Text in
        match Error.to_exn e with
        | exception Syntax_error { filename; token; position } ->
            print_endline
              (sprintf "Syntax error in %s at %s" filename
                 (Position.to_string position));
            print_endline (sprintf "Unexpected token %s" token)
        | e -> print_endline (Exn.to_string e))
end
