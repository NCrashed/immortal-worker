let
  nixpkgs = import ./pkgs.nix;
  project = import (import ./project.nix) { inherit nixpkgs; };
in project {
  packages = {
    immortal-worker = ../.;
  };
  shellTools = pkgs: with pkgs.haskellPackages; [
    cabal-install
    ghcid
  ];
  compiler = "ghc8102";
}
