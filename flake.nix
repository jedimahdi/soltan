{
  description = "Soltan";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default =
          pkgs.mkShell {
            buildInputs = with pkgs; [ zlib ];
            nativeBuildInputs = [ ];
          };
      });
}
