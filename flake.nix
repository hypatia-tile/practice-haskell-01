{
  description = "Haskell development environment for the experiment";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        hpkgs = pkgs.haskellPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            (hpkgs.ghcWithPackages (p: [
              p.hoogle
              p.transformers
              p.vector
              p.bytestring
              p.megaparsec
            ]))
            hpkgs.cabal-install
            hpkgs.haskell-language-server
            hpkgs.fourmolu
            hpkgs.fast-tags
          ];
        };
      }
    );
}
