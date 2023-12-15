{
  description = "Soltan Game";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
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
        devShell =
          pkgs.project.haskellPackages.shellFor
            {
              name = "soltan";
              packages = _: [ ];
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
