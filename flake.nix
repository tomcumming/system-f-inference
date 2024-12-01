{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
  };
  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages."${system}";
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = [
          pkgs.haskell.compiler.ghc98
          pkgs.cabal-install
          (pkgs.haskell-language-server.override { supportedGhcVersions = [ "98" ]; })
          pkgs.ormolu
        ];
      };
      formatter."${system}" = pkgs.nixpkgs-fmt;
    };
}
