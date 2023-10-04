open Core

module Version = struct
  type t = Version of { major : int; minor : int; patch : int; build : int }
  [@@deriving compare, sexp]

  let default = Version { major = 0; minor = 0; patch = 0; build = 0 }
  let create major minor patch build = Version { major; minor; patch; build }

  let to_string (Version { major; minor; patch; build }) =
    sprintf "%d.%d.%d.%d" major minor patch build
end

module type T = sig
  val run : string list -> unit
end

module type S = sig
  val executable_name : string
  val version : Version.t
end

module Make (S : S) : T = struct
  let version = Version.to_string S.version
  let make_runner (module S : Runner.S) = (module Runner.Make (S) : Runner.T)

  let command =
    Command.basic ~summary:"Fx Programming Language"
      [%map_open.Command
        let print_ast = flag "-dump-ast" no_arg ~doc:"FILE dump ast to file"
        (* TODO: make type simplification 'optional' *)
        and print_tast = flag "-dump-tast" no_arg ~doc:"FILE infer type of file"
        and filename =
          (* FIXME: '-' is not a real file, should drop into 'repl' mode *)
          anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
        in
        let (module Cli_runner) =
          make_runner
            (module struct
              let print_ast = print_ast
              let print_tast = print_tast
              let filename = filename
            end)
        in
        Cli_runner.run]

  let run argv =
    Command_unix.run ~version
      ~argv:(S.executable_name :: argv)
      ~build_info:"FXL" command
end

module Tests : sig end = struct
  let run_it executable_name argv =
    let module T = Make (struct
      let executable_name = executable_name
      let version = Version.default
    end) in
    T.run argv

  module Tempfile (T : sig
    val prefix : string
    val suffix : string
  end) : sig
    val with_file : string -> (string -> 'a) -> 'a
  end = struct
    open Filename_unix

    let create () = temp_file T.prefix T.suffix

    let write_to filename content =
      let oc = Out_channel.create filename in
      Out_channel.output_string oc content;
      Out_channel.close oc

    let with_file content f =
      let filename = create () in
      write_to filename content;
      let result = f filename in
      Sys_unix.remove filename;
      result
  end

  module MakeFxTemp (T : sig
    val prefix : string
  end) =
  Tempfile (struct
    let prefix = T.prefix
    let suffix = "fx"
  end)

  let%expect_test "cli help" =
    run_it "test-cli" [ "-help" ];
    [%expect
      {|
    Fx Programming Language

      test-cli [FILENAME]

    === flags ===

      [-dump-ast]                . FILE dump ast to file
      [-dump-tast]               . FILE infer type of file
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command/src/command.ml.Exit_called (status 0))
    |}]

  let%expect_test "cli version default" =
    run_it "test-cli" [ "-version" ];
    [%expect
      {|
   0.0.0.0
   (command/src/command.ml.Exit_called (status 0))
    |}]

  let%expect_test "cli version" =
    let module C = Make (struct
      let executable_name = "test-cli"
      let version = Version.create 1 2 3 4
    end) in
    C.run [ "-version" ];
    [%expect
      {|
   1.2.3.4
   (command/src/command.ml.Exit_called (status 0))
    |}]

  let%expect_test "ast dump no file" =
    run_it "printer" [ "-dump-ast" ];
    [%expect {| ("Fx__Runner.File_not_found(\"-\")") |}]

  let%expect_test "build_info" =
    run_it "test-cli" [ "-build-info" ];
    [%expect
      {|
   FXL
   (command/src/command.ml.Exit_called (status 0))
    |}]

  let%expect_test "cli has dump-ast" =
    let module Temp = MakeFxTemp (struct
      let prefix = "print"
    end) in
    Temp.with_file "" (fun file -> run_it "printer" [ "-dump-ast"; file ]);
    [%expect
      {| (Uint(value 0)(span(Span(filename"")(start(Position(line 0)(column 0)))(finish(Position(line 0)(column 0)))))) |}]

  let%expect_test "ast dump nonsense" =
    let module Temp = MakeFxTemp (struct
      let prefix = "print"
    end) in
    Temp.with_file "- 9 asdasd--" (fun file ->
        run_it "printer" [ "-dump-ast"; file ]);
    [%expect {| ("Fx__Lexer.SyntaxError(\"\", \"-\", _)") |}]

  let%expect_test "ast dump valid" =
    let module Temp = MakeFxTemp (struct
      let prefix = "print"
    end) in
    Temp.with_file "42" (fun file -> run_it "printer" [ "-dump-ast"; file ]);
    [%expect
      {| (Uint(value 42)(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 2)))))) |}]

  let%expect_test "type ast" =
    let module Temp = MakeFxTemp (struct
      let prefix = "print"
    end) in
    Temp.with_file "42" (fun file ->
        run_it "printer tast" [ "-dump-tast"; file ]);
    [%expect
      {| (Tint(value 42)(span(Span(filename"")(start(Position(line 1)(column 0)))(finish(Position(line 1)(column 2)))))) |}]
end
