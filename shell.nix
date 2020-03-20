with import <nixpkgs> {};

let
  sources = import ./nix/sources.nix;
  mozilla = callPackage "${sources.mozilla}/package-set.nix" {};
in mkShell {
  nativeBuildInputs = [
    mozilla.latest.rustChannels.stable.rust
  ];
}
