with (import <nixpkgs> {});
mkShell {
  name = "arche-fronted";
  buildInputs = [
    gnumake
    nodejs
    nodePackages.npm
    elmPackages.elm
    elmPackages.create-elm-app
  ];
}

