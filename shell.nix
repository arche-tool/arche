with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    gnumake
    docker
    google-cloud-sdk
    stack
  ];
}