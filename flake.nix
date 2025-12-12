{
  description = "Dev shell for the language project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            rustc
            cargo
            rustfmt
            clippy
            pkg-config
            openssl
            openssl.dev
          ];

          shellHook = ''
            echo "Loaded dev shell: rustc=$(${pkgs.rustc}/bin/rustc --version)"
          '';
        };
      });
}
