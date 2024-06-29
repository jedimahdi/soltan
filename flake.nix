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
          let buildInputs = [ pkgs.zlib ];
          in
          pkgs.mkShell {
            inherit buildInputs;
            nativeBuildInputs = [ pkgs.just pkgs.ghcid ];
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
          };
      });
}
