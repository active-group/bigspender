{
  description = "FUNAR";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, ... }:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        # Pin GHC version for easier, explicit upgrades later
        ghcVersion = "945";
        pkgs = import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            (_: prev: {
              haskellPackages = prev.haskell.packages."ghc${ghcVersion}";
            })
          ];
        };
        bigspender = pkgs.haskellPackages.callCabal2nix "bigspender"
          (pkgs.lib.cleanSource ./.) { };
      in {
        devShells = {
          default = pkgs.haskellPackages.shellFor {
            packages = _: [ bigspender ];
            buildInputs = [
              pkgs.cabal-install
              self.packages.${system}.hls
            ];
            nativeBuildInputs = [
              pkgs.haskellPackages.doctest
            ];
            shellHook = ''
              export PS1="\n\[\033[1;32m\][nix-shell:\W \[\033[1;31m\]FUNAR\[\033[1;32m\]]\$\[\033[0m\] "
              echo -e "\n\033[1;31m ♣ ♠ Welcome to BigSpender! ♥ ♦ \033[0m\n"
              echo -e "   Use the following command to open VSCode in this directory:\n"
              echo "       code ."
            '';
          };

          withVSCode = self.devShells.${system}.default.overrideAttrs (old:
            let
              vscode = pkgs.vscode-with-extensions.override {
                vscodeExtensions = with pkgs.vscode-extensions; [
                  bbenoist.nix
                  haskell.haskell
                  justusadam.language-haskell
                ];
              };
            in {
              buildInputs = old.buildInputs ++ [ vscode ];
              shellHook = old.shellHook + ''
                echo -e "\n   All required extensions should be pre-installed and ready."'';
            });
        };

        packages = {
          inherit bigspender;
          inherit (pkgs) ghc cabal-install;
          hls = pkgs.haskell-language-server.override {
            supportedGhcVersions = [ ghcVersion ];
          };
        };
      });
}
