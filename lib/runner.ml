open Core

module type T = sig
  val run : unit -> unit
end

module type S = sig
  val print_ast : bool
  val filename : string
end

exception File_not_found of string

module Make (S : S) : T = struct
  module Syntax = struct
    include Syntax
    include To_syntax
  end

  let print_ast = S.print_ast
  let filename = S.filename

  let protect f =
    match f () with
    | () -> ()
    | exception File_not_found filename -> printf "File %s not found\n" filename
    | exception e -> printf "Error: %s\n" (Exn.to_string e)

  let f () =
    filename
    |> (fun filename ->
         match Sys_unix.file_exists filename with
         | `Yes -> S.filename
         | `No | `Unknown -> raise (File_not_found filename))
    |> In_channel.read_all |> Syntax.parse
    |> (fun ast ->
         if print_ast then print_endline (Sexp.to_string (Syntax.sexp_of_t ast));
         ast)
    |> ignore

  let run () = protect f
end
