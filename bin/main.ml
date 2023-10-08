open Core
open Fx

let main argv =
  let argv = Array.to_list argv in
  make_cli () argv

let () = main (Sys.get_argv ())
