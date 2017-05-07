{ package, test ? true, frontend ? false }:
let bootstrap = import <nixpkgs> {};
    fetch-github-json = owner: repo: path:
      let commit = builtins.fromJSON (builtins.readFile path);
      in bootstrap.fetchFromGitHub {
            inherit owner repo;
            inherit (commit) rev sha256;
      };
    reflex-platform = import (fetch-github-json "reflex-frp" "reflex-platform" ./reflex-platform.json) {};
    compiler = if frontend then "ghcjs" else "ghc";
    overrides = (builtins.getAttr compiler reflex-platform).override {
      overrides = self: super:
        with reflex-platform;
        let options = pkg: lib.overrideCabal pkg (drv: { doCheck = test; });
            filterPredicate = p: type:
              let path = baseNameOf p; in
              !builtins.any (x: x)
              [(type == "directory" && path == "dist")
               (type == "symlink"   && path == "result")
               (type == "directory" && path == ".git")];
        in {
          mkDerivation = args: super.mkDerivation (args //
            (if nixpkgs.stdenv.isDarwin && !frontend then {
              postCompileBuildDriver = ''
                echo "Patching dynamic library dependencies"
                # 1. Link all dylibs from 'dynamic-library-dirs's in package confs to $out/lib/links
                mkdir -p $out/lib/links
                for d in $(grep dynamic-library-dirs $packageConfDir/*|awk '{print $2}'); do
                  ln -s $d/*.dylib $out/lib/links
                done

                # 2. Patch 'dynamic-library-dirs' in package confs to point to the symlink dir
                for f in $packageConfDir/*.conf; do
                  sed -i "s,dynamic-library-dirs: .*,dynamic-library-dirs: $out/lib/links," $f
                done

                # 3. Recache package database
                ghc-pkg --package-db="$packageConfDir" recache
              '';
            } else {}));
        } // import ./overrides.nix { inherit options filterPredicate lib cabal2nixResult self super; };
    };
    drv = builtins.getAttr package overrides;
in if reflex-platform.nixpkgs.lib.inNixShell then
  reflex-platform.workOn overrides drv
else
  drv
