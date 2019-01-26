{
  nodejsVersion ? "10.14.2",
  yarnVersion ? "1.12.3",
  purescriptVersion ? "0.12.1",
  electronVersion ? "3.0.0-beta.5",
  nixjs ? fetchTarball "https://github.com/cprussin/nixjs/archive/0.0.4.tar.gz",
  nixpkgs ? <nixpkgs>
}:

with import nixpkgs {
  overlays = [
    (import nixjs {
      nodejs = nodejsVersion;
      yarn = yarnVersion;
      purescript = purescriptVersion;
      electron = electronVersion;
    })
  ];
};

let
  setup = "export PATH=$PATH:$PWD/node_modules/.bin";
in

stdenv.mkDerivation {
  name = "minichrome";
  buildInputs = [
    git
    nodejs
    yarn
    purescript
    electron
  ];
  src = ./.;
  preConfigure = setup;
  shellHook = setup;
}
