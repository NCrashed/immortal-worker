# To update nix-prefetch-git https://github.com/NixOS/nixpkgs-channels
import ((import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs-channels";
  rev = "e0759a49733dfc3aa225b8a7423c00da6e1ecb67";
  sha256  = "1lnaifrbdmvbmz25404z7xpfwaagscs1i80805fyrrs1g27h21qb";
})
