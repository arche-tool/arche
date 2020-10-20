with (import <nixpkgs> {});
mkShell {
  name = "arche-backend";
  buildInputs = [
    gnumake
    docker
    google-cloud-sdk
    stack
  ];
}