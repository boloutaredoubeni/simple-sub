{
  description = "Fx";

  # Flake dependency specification
  #
  # To update all flake inputs:
  #
  #     $ nix flake update --commit-lockfile
  #
  # To update individual flake inputs:
  #
  #     $ nix flake lock --update-input <input> ... --commit-lockfile
  #
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nix2container.url = "github:nlewo/nix2container";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
    mk-shell-bin.url = "github:rrbutani/nix-mk-shell-bin";
    # Precisely filter files copied to the nix store
    nix-filter.url = "github:numtide/nix-filter";
    gitignore.url = "github:hercules-ci/gitignore.nix";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs =
    inputs @ { flake-parts
    , nix-filter
    , pre-commit-hooks
    , ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ ];
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      perSystem =
        { self'
        , config
        , pkgs
        , system
        , ...
        }:
        let
          # Legacy packages that have not been converted to flakes
          # OCaml packages available on nixpkgs
          ocamlPackages =
            pkgs.ocaml-ng.ocamlPackages_5_0.overrideScope'
              (_self: super: {
                ocaml = super.ocaml.override { flambdaSupport = true; };
              });

          # Library functions from nixpkgs

          # Filtered sources (prevents unecessary rebuilds)
          sources = {
            ocaml = nix-filter.lib {
              root = ./.;
              include = [
                ".ocamlformat"
                "dune-project"
                "dune"
                "fx.opam"
                (nix-filter.lib.matchExt "h")
                (nix-filter.lib.matchExt "ml")
                (nix-filter.lib.matchExt "mli")
                (nix-filter.lib.matchExt "mly")
                (nix-filter.lib.matchExt "mll")
                (nix-filter.lib.inDirectory "bin")
                (nix-filter.lib.inDirectory "lib")
                (nix-filter.lib.inDirectory "test")
              ];
            };

            nix = nix-filter.lib {
              root = ./.;
              include = [ (nix-filter.lib.matchExt "nix") ];
            };
          };

          fx-deps = with ocamlPackages; [
            menhir
            wasm
            core
            base
            stdio
            ppx_inline_test
            ppx_assert
            ppx_expect
            ppx_jane
            core_unix
          ];

          fx-dev = with ocamlPackages; [
            odoc
            # OCaml editor support
            ocaml-lsp
            # Nicely formatted types on hover
            ocamlformat-rpc-lib
            # Fancy REPL thing
            utop
            pkgs.opam

            dune_3
            menhir
          ];
          # TODO: support hydra builds
          # TODO: support docker builds
        in
        {
          # Exposed packages that can be built or run with `nix build` or
          # `nix run` respectively:
          #
          #     $ nix build .#<name>
          #     $ nix run .#<name> -- <args?>
          #
          packages = {
            # The package that will be built or run by default. For example:
            #
            #     $ nix build
            #     $ nix run -- <args?>
            #
            default = ocamlPackages.buildDunePackage {
              pname = "fx";
              version = "0.1.0";
              duneVersion = "3";
              src = sources.ocaml;
              buildInputs = fx-deps;
              nativeBuildInputs = fx-dev;
              doCheck = true;
              checkPhase = ''
                dune runtest
              '';
            };
          };

          checks = {
            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              hooks = {
                nixpkgs-fmt.enable = true;
                yamllint.enable = true;
                statix.enable = true;
                deadnix.enable = true;
                dune-fmt.enable = true;
                dune-test = {
                  enable = true;
                  name = "dune test";
                  entry = "dune runtest";
                  pass_filenames = false;
                };
              };
            };

            # Run tests for the `fx` package
            fx =
              let
                # Patches calls to dune commands to produce log-friendly output
                # when using `nix ... --print-build-log`. Ideally there would be
                # support for one or more of the following:
                #
                # In Dune:
                #
                # - have workspace-specific dune configuration files
                #
                # In NixPkgs:
                #
                # - allow dune flags to be set in in `pkgs.ocamlPackages.buildDunePackage`
                # - alter `pkgs.ocamlPackages.buildDunePackage` to use `--display=short`
                # - alter `pkgs.ocamlPackages.buildDunePackage` to allow `--config-file=FILE` to be set
                patchDuneCommand =
                  let
                    subcmds = [ "build" "test" "runtest" "install" ];
                  in
                  pkgs.lib.replaceStrings
                    (pkgs.lib.lists.map (subcmd: "dune ${subcmd}") subcmds)
                    (pkgs.lib.lists.map (subcmd: "dune ${subcmd} --display=short")
                      subcmds);
              in
              self'.packages.${system}.fx.overrideAttrs (oldAttrs: {
                name = "check-${oldAttrs.name}";
                doCheck = true;
                buildPhase = patchDuneCommand oldAttrs.buildPhase;
                checkPhase = patchDuneCommand oldAttrs.checkPhase;
                # skip installation (this will be tested in the `fx-app` check)
                installPhase = "echo 'skipping install'";
              });

            # Check Dune and OCaml formatting
            dune-fmt =
              pkgs.runCommand "check-dune-fmt"
                {
                  nativeBuildInputs = [ pkgs.ocamlformat ] ++ fx-deps;
                } ''
                echo "checking dune and ocaml formatting"
                dune build \
                  --display=short \
                  --no-print-directory \
                  --root="${sources.ocaml}" \
                  --build-dir="$(pwd)/_build" \
                  @fmt
                touch $out
              '';

            # Check documentation generation
            dune-doc =
              pkgs.runCommand "check-dune-doc"
                {
                  ODOC_WARN_ERROR = "true";
                  nativeBuildInputs = [ pkgs.ocamlPackages.odoc ] ++ fx-deps;
                } ''
                echo "checking ocaml documentation"
                dune build \
                  --display=short \
                  --no-print-directory \
                  --root="${sources.ocaml}" \
                  --build-dir="$(pwd)/_build" \
                  @doc
                touch $out
              '';
          };

          # Development shells
          #
          #    $ nix develop .#<name>
          #    $ nix develop .#<name> --command dune build @test
          #    $ nix develop .#<name> --command dune utop lib -- -implicit-bindings
          #
          # [Direnv](https://direnv.net/) is recommended for automatically loading
          # development environments in your shell. For example:
          #
          #    $ echo "use flake" > .envrc && direnv allow
          #    $ dune build @test
          #
          # devenv.shells.default = mk-shell ocamlPackages;
          devShells = {
            default = pkgs.mkShell {
              inherit (self'.checks.pre-commit-check) shellHook;

              packages = with pkgs;
                [
                  # Source file formatting
                  pkgs.nixpkgs-fmt
                  pkgs.ocamlformat
                  # For `dune build --watch ...`
                  pkgs.fswatch
                  # For `dune build @doc`
                  ocamlPackages.odoc
                  # OCaml editor support
                  ocamlPackages.ocaml-lsp
                  # Nicely formatted types on hover
                  ocamlPackages.ocamlformat-rpc-lib
                  # Fancy REPL thing
                  ocamlPackages.utop
                  pkgs.act
                ]
                ++ fx-deps
                ++ fx-dev;

              inputsFrom = [ config.packages.default ];
            };
          };
        };
    };
}
