{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "45ffca6ff84ca5e00842900802af577dcfb3e84f";
    sha256 = "11vnmlix4xkifrlpz4a13r6dnncrwnjibnd2j5sl7zb9vklj40lc";
  }) {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;
in { miso-release = import ./default.nix { nixpkgs = pkgs; };
     miso-ghcjs8 = ghcjsHEAD.callPackage ./miso-ghcjs.nix {};
     haskell-miso.org = import ./examples/haskell-miso.org {};
     sse-example = import ./examples/sse {};
   }
