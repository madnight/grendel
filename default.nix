{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-qq, async, attoparsec, base
      , bytestring, http-client, http-client-tls, http-conduit
      , http-types, lens, monad-parallel, scotty, split, stdenv
      , string-conversions, template-haskell, text, time, transformers
      , unordered-containers, utf8-string, wreq
      }:
      mkDerivation {
        pname = "grendel";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base lens string-conversions template-haskell utf8-string
          wreq
        ];
        executableHaskellDepends = [
          aeson aeson-qq async attoparsec base bytestring http-client
          http-client-tls http-conduit http-types lens monad-parallel scotty
          split string-conversions template-haskell text time transformers
          unordered-containers utf8-string wreq
        ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/githubuser/grendel#readme";
        license = stdenv.lib.licenses.bsd3;
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        configureFlags = [
          "--ghc-option=-optl=-pthread"
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
          "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
        ];
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
