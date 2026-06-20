final: prev: let
  # darktable-org/rawspeed fork rev the ARW6 (Sony LLVC v3) lossy decoder was
  # developed and verified bit-exact against the Adobe DNG Converter oracle.
  rawspeedRev = "7cf3dc3b9d9c82b414198b1f57460478be6c6c9d";
  rawspeedSrc = final.fetchFromGitHub {
    owner = "darktable-org";
    repo = "rawspeed";
    rev = rawspeedRev;
    hash = "sha256-v8spw3yqZFIVaIVPBJH/lURWTKE3Zye2OCEeULTSTrg=";
  };
in {
  deadbeef-with-plugins = prev.deadbeef-with-plugins.override {
    plugins = [final.deadbeef-mpris2-plugin];
  };
  wine = final.wineStaging;

  # Sony a7R VI (ILCE-7RM6) ARW support, now including the lossy "Compressed HQ"
  # format (Sony LLVC v3, raw SubIFD compression 32766 / arw6), reverse-
  # engineered from the Adobe DNG Converter and ported from a prototype verified
  # bit-exact against its output.
  #
  #  * patches/*-a7rm6.patch  add the camera definition (lossless + lossy); the
  #    color matrices are the upstream ILCE-7RM5 (a7R V) values, provisional.
  #  * patches/*-arw6.patch   add the reverse-engineered lossy arw6 decoder.
  #
  # rawspeed isn't packaged standalone (darktable vendors it in its release
  # tarball), so we both expose a standalone `rawspeed` and apply the same patch
  # to darktable's bundled copy (src/external/rawspeed, rev 4c511d6 for 5.4.1).
  # dnglab is bumped from v0.7.2 to upstream main (0a6f954): v0.7.2 predates the
  # arw6 module scaffolding the lossy decoder is built on, so the patch can only
  # apply to main. The two hashes track that bumped source.

  rawspeed = final.stdenv.mkDerivation {
    pname = "rawspeed";
    version = "git-${builtins.substring 0 7 rawspeedRev}-arw6";
    src = rawspeedSrc;
    patches = [./patches/rawspeed-arw6.patch];
    nativeBuildInputs = [final.cmake final.ninja final.pkg-config];
    buildInputs =
      [final.pugixml final.libjpeg final.zlib]
      ++ final.lib.optional final.stdenv.cc.isClang final.llvmPackages.openmp;
    cmakeFlags = [
      "-DBUILD_TESTING=OFF"
      "-DBUILD_BENCHMARKING=OFF"
      "-DBUILD_FUZZERS=OFF"
      "-DBUILD_DOCS=OFF"
      "-DBUILD_TOOLS=ON"
      "-DUSE_XMLLINT=OFF"
      "-DRAWSPEED_ENABLE_WERROR=OFF"
    ];
    # rawspeed installs only the camera DB + `identify`; ship the lib + headers.
    postInstall = ''
      mkdir -p $out/lib
      find . -name 'librawspeed*.a' -exec cp -t $out/lib {} +
      (cd "$src/src" && find librawspeed -name '*.h' \
        -exec install -Dm644 {} "$out/include/{}" \;)
      find . -name 'rawspeedconfig.h' \
        -exec install -Dm644 {} "$out/include/librawspeed/rawspeedconfig.h" \;
    '';
    meta = with final.lib; {
      description = "RAW decoding library (darktable-org/rawspeed) with Sony ARW6 / LLVC v3 support";
      homepage = "https://github.com/darktable-org/rawspeed";
      license = licenses.lgpl21Plus;
      platforms = platforms.unix;
    };
  };

  darktable = prev.darktable.overrideAttrs (old: {
    # Lossless a7R VI: cameras.xml entry + raised LJPEG dimension guard.
    patches =
      (old.patches or [])
      ++ [
        ./patches/darktable-rawspeed-a7rm6.patch
      ];
    # Lossy arw6 decoder, applied in-place to the bundled rawspeed.
    postPatch =
      (old.postPatch or "")
      + ''
        echo "Applying Sony ARW6 (LLVC v3) lossy decoder to bundled rawspeed..."
        patch -p1 -d src/external/rawspeed < ${./patches/rawspeed-arw6.patch}
      '';
    cmakeFlags = (old.cmakeFlags or []) ++ ["-DRAWSPEED_ENABLE_WERROR=OFF"];
  });

  dnglab = final.rustPlatform.buildRustPackage (finalAttrs: {
    pname = "dnglab";
    version = "0.7.2-arw6-g0a6f954";
    src = final.fetchFromGitHub {
      owner = "dnglab";
      repo = "dnglab";
      rev = "0a6f95496cfaf3c61d9bb03ec27a025e1935d600";
      # darwin/linux hash mismatch (same cleanup nixpkgs' dnglab uses).
      postFetch = ''
        rm -rf "$out"/rawler/data/testdata/cameras/Canon/{"EOS REBEL T7i","EOS Rebel T7i"}
      '';
      hash = "sha256-hjDhZEc3m+vha+Te27iXAtBT9kuop35CeFsmFMXYQbc=";
    };
    cargoHash = "sha256-li+5UIbJi2cOEgAKsW/x2ShIWeoy7IEdAmJZFqrEKiA=";
    patches = [
      ./patches/dnglab-arw6.patch # lossy arw6 decoder
      ./patches/dnglab-a7rm6.patch # a7R VI camera entry
    ];
    postInstall = ''
      rm -f $out/bin/benchmark $out/bin/identify
    '';
    meta =
      prev.dnglab.meta
      // {
        description = "Camera RAW to DNG converter (with Sony ARW6 / LLVC v3 support)";
      };
  });

  /*
   mullvad = prev.mullvad.overrideAttrs (old: {
    patches =
      old.patches or []
      ++ [
        ./0001-Set-base-rule-priority.patch
      ];
  });
  */
}
