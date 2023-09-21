module type T = sig
  val run : unit -> unit
end

module type S = sig
  val print_ast : bool
  val filename : string
end

module Make (Config : S) : T = struct
  (* let print_ast = Config.print_ast
     let filename = Config.filename *)
  let run () = ()
  (* printf "print_ast: %b\n%s" print_ast filename *)
end
