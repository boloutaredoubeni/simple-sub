let make_cli () argv =
  let module S : Cli.S = struct
    let version = Cli.Version.create 0 1 0 0
    let executable_name = "simple-sub"
  end in
  let module F : Cli.T = Cli.Make (S) in
  F.run argv
