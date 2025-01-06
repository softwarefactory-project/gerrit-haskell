{
  description = "gerrit-haskell";
  nixConfig.bash-prompt = "[nix(gerrit-haskell)] ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/d3780c92e64472e8f9aa54f7bbb0dd4483b98303";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        myPackage =
          pkgs.haskellPackages.callCabal2nix "gerrit-haskell" self { };

      in {
        defaultPackage = myPackage;

        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ myPackage ];

          buildInputs = [
            pkgs.ghcid
            pkgs.haskellPackages.fourmolu
            pkgs.cabal-install
            pkgs.hlint
            pkgs.haskell-language-server
          ];
        };
      });
}
