with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    gnumake
    nodejs
    nodePackages.npm
    elmPackages.create-elm-app
  ];
}

