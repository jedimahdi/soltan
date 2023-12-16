{
  description = "Soltan Game";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "utils";
    };
  };

  outputs = { self, nixpkgs, utils, pre-commit-hooks, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (final: _: { project.haskellPackages = final.haskell.packages.ghc948; })
          ];
        };
      in
      {
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
            };
          };
        };
        devShells.default =
          pkgs.project.haskellPackages.shellFor
            {
              name = "soltan";
              packages = _: [ ];
              inherit (self.checks.${system}.pre-commit-check) shellHook;
              buildInputs = with pkgs; [ zlib.dev ];
              nativeBuildInputs = builtins.concatMap
                builtins.attrValues
                [
                  ###################################################
                  # Code styles:
                  {
                    inherit (pkgs) hlint nixpkgs-fmt stylish-haskell;
                    inherit (pkgs.python3Packages) yamllint;
                    inherit (pkgs.nodePackages) prettier;
                  }

                  ###################################################
                  # Command line tools:
                  {
                    inherit (pkgs) entr ghcid git;
                  }

                  ###################################################
                  # Language servers:
                  {
                    inherit (pkgs) haskell-language-server;
                  }

                  ###################################################
                  # Package managers:
                  { inherit (pkgs) cabal-install; }
                ];
            };
      });
}
