let unstable = import (builtins.fetchTarball https://github.com/nixos/nixpkgs/tarball/08bb60901d56ae96e3da54edd46eeb3436227f0) {};
in
{ pkgs ? import <nixpkgs>
  {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    jdk11_headless
    unstable.clojure
    unstable.clojure-lsp
    babashka
    clj-kondo
    nasm
  ];
}
