{
  description = "A flake demonstrating how to build OCaml projects with Dune";

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
    devenv.url = "github:cachix/devenv";
    nix2container.url = "github:nlewo/nix2container";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
    mk-shell-bin.url = "github:rrbutani/nix-mk-shell-bin";
    # Convenience functions for writing flakes
    flake-utils.url = "github:numtide/flake-utils";
    # Precisely filter files copied to the nix store
    nix-filter.url = "github:numtide/nix-filter";
  };

  nixConfig = {
    extra-trusted-public-keys =
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };

  outputs = inputs@{ flake-parts, nix-filter, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.devenv.flakeModule ];
      systems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      perSystem = { config, pkgs, ... }:
        let
          # Legacy packages that have not been converted to flakes
          # OCaml packages available on nixpkgs
          ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14.overrideScope'
            (_self: super: {
              ocaml = super.ocaml.override { flambdaSupport = true; };
            });

          ocaml5Packages = pkgs.ocaml-ng.ocamlPackages_5_0.overrideScope'
            (_self: super: {
              ocaml = super.ocaml.override { flambdaSupport = true; };
            });

          fx-deps = op: with op; [ menhir ];

          fx-dev = op:
            with op; [
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

          mk-fx = ocamlPackages:
            ocamlPackages.buildDunePackage {
              pname = "fx";
              version = "0.1.0";
              duneVersion = "3";
              src = sources.ocaml;
              buildInputs = fx-deps ocamlPackages;
              nativeBuildInputs = fx-dev ocamlPackages;
              doCheck = true;
            };

          mk-shell = ocamlPackages: {
            name = "fx";

            devcontainer.enable = true;
            devcontainer.settings.updateContentCommand = "";

            difftastic.enable = true;

            languages = {
              ocaml = {
                enable = true;
                packages = ocamlPackages;
              };
            };

            pre-commit.settings.deadnix.edit = true;

            pre-commit.hooks = {
              # nix specific
              nixfmt.enable = true;
              statix.enable = true;
              deadnix.enable = true;
            };

            languages.nix.enable = true;

            # TODO: add a custom pre-commit hook to run `dune build @fmt`

            # Development tools
            packages = [
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

              pkgs.clang_16

              pkgs.act
            ] ++ fx-deps ocamlPackages ++ fx-dev ocamlPackages;
          };
        in {
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
            default = config.packages.fx;

            fx = mk-fx ocamlPackages;
            fx-multicore = mk-fx ocaml5Packages;
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
          devenv.shells.default = mk-shell ocamlPackages;
          devenv.shells.multicore = mk-shell ocaml5Packages;
        };
    };
}
