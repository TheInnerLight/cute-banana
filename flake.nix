{
  description = "CuteBanana";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;
        hlib = pkgs.haskell.lib.compose;

        ghcVersion = "ghc96";

        qt = pkgs.qt6;

        withQt = drv:
          lib.pipe drv [
            hlib.unmarkBroken
            hlib.doJailbreak
            hlib.dontCheck
            (hlib.addBuildTools [ qt.qtbase pkgs.pkg-config ])
            (hlib.addBuildDepends [ qt.qtbase ])
            (hlib.overrideCabal (old: {
              preConfigure = (old.preConfigure or "") + ''
                export QTAH_QT=6
              '';
            }))
            (drv: drv.overrideAttrs (_: { dontWrapQtApps = true; }))
          ];

        relax = drv: lib.pipe drv [ hlib.unmarkBroken hlib.doJailbreak hlib.dontCheck ];

        haskellPackages = pkgs.haskell.packages.${ghcVersion}.override {
          overrides = hself: hsuper: {
            hoppy-generator = relax hsuper.hoppy-generator;
            hoppy-runtime   = relax hsuper.hoppy-runtime;
            hoppy-std       = relax hsuper.hoppy-std;

            qtah-generator = withQt hsuper.qtah-generator;
            qtah-cpp-qt6   = withQt hsuper.qtah-cpp-qt6;
            qtah-qt6       = withQt hsuper.qtah-qt6;

            cute-banana = (hself.callCabal2nix "cute-banana" ./. { }).overrideAttrs (_: {
              dontWrapQtApps = true;
            });
          };
        };

        qtPluginPath = "${qt.qtbase}/${qt.qtbase.qtPluginPrefix}";

        app = pkgs.stdenv.mkDerivation {
          pname = "cute-banana";
          version = "0.1.0";
          dontUnpack = true;
          nativeBuildInputs = [ qt.wrapQtAppsHook ];
          buildInputs = [ qt.qtbase ] ++ lib.optional pkgs.stdenv.isLinux qt.qtwayland;
          installPhase = ''
            mkdir -p $out/bin
            cp ${haskellPackages.cute-banana}/bin/cute-banana $out/bin/
          '';
        };
      in
      {
        packages = {
          default = app;
          unwrapped = haskellPackages.cute-banana;
        };

        apps.default = flake-utils.lib.mkApp { drv = app; };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.cute-banana ];
          withHoogle = true;

          nativeBuildInputs = [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
            haskellPackages.qtah-generator
            pkgs.pkg-config
          ];

          buildInputs = [ qt.qtbase qt.qtsvg ]
            ++ lib.optional pkgs.stdenv.isLinux qt.qtwayland;

          QTAH_QT = "6";
          QT_PLUGIN_PATH = qtPluginPath;
          QT_QPA_PLATFORM_PLUGIN_PATH = "${qtPluginPath}/platforms";

        };

        formatter = pkgs.nixpkgs-fmt;
      });
}
