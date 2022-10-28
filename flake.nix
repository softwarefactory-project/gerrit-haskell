{
  description = "gerrit-haskell";
  nixConfig.bash-prompt = "[nix(gerrit-haskell)] ";

  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/8596beefeb8471cbafae9bdefb6cb5de8dbc5627";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, hspkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        packageName = "gerrit-haskell";
        pkgs = hspkgs.pkgs;
        haskellPackages = pkgs.hspkgs;
        myPackage = haskellPackages.callCabal2nix packageName self { };

      in {
        defaultPackage = myPackage;

        devShell = haskellPackages.shellFor {
          packages = p: [ myPackage ];

          buildInputs = [
            pkgs.ghcid
            pkgs.ormolu
            pkgs.cabal-install
            pkgs.hlint
            pkgs.haskell-language-server
          ];
        };
      });
}
