{
  nodejs ? "10.15.1",
  yarn ? "1.12.3",
  purescript ? "0.12.2",
  electron ? "3.0.0-beta.5",
  nixjs-version ? "0.0.7",
  nixjs ? fetchTarball "https://github.com/cprussin/nixjs/archive/${nixjs-version}.tar.gz",
  nixpkgs ? <nixpkgs>
}:


let
  nixjs-overlay = import nixjs { inherit nodejs yarn purescript electron; };
  pkgs = import nixpkgs { overlays = [ nixjs-overlay ]; };
  setup = "export PATH=$PATH:$PWD/node_modules/.bin";
in

pkgs.stdenv.mkDerivation {
  name = "minichrome";
  buildInputs = [
    pkgs.git
    pkgs.nodejs
    pkgs.yarn
    pkgs.purescript
    pkgs.electron
    pkgs.psc-package
  ];
  src = ./.;
  preConfigure = setup;
  shellHook = setup;
}
