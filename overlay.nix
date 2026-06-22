{withLibRaw ? true}: final: prev: {
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

  # darktable vendors rawspeed in its release tarball (src/external/rawspeed, rev
  # 4c511d6 for 5.4.1); patch it in place: the lossless camera entry via `patches`,
  # the lossy decoder via `postPatch`.
  darktable = prev.darktable.overrideAttrs (old: {
    patches =
      (old.patches or [])
      ++ [
        ./patches/darktable-rawspeed-a7rm6.patch
      ];
    postPatch =
      (old.postPatch or "")
      + ''
        echo "Applying Sony ARW6 (LLVC v3) lossy decoder to bundled rawspeed..."
        patch -p1 -d src/external/rawspeed < ${./patches/rawspeed-arw6.patch}
      '';
    cmakeFlags = (old.cmakeFlags or []) ++ ["-DRAWSPEED_ENABLE_WERROR=OFF"];
  });

  # dnglab: v0.7.2 predates the arw6 module scaffolding the lossy decoder builds
  # on, so override src to upstream main (0a6f954). buildRustPackage vendors deps
  # from the original src + cargoHash, so overriding src also requires overriding
  # cargoDeps (the new lockfile's vendor) — patches add no deps, so only src/hash
  # and patches change; the rest of the package definition is kept.
  dnglab = prev.dnglab.overrideAttrs (
    old: let
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
    in {
      inherit src;
      cargoDeps = final.rustPlatform.fetchCargoVendor {
        name = "dnglab-arw6-g0a6f954-vendor";
        inherit src;
        hash = "sha256-li+5UIbJi2cOEgAKsW/x2ShIWeoy7IEdAmJZFqrEKiA=";
      };
      patches =
        (old.patches or [])
        ++ [
          ./patches/dnglab-arw6.patch # lossy arw6 decoder
          ./patches/dnglab-a7rm6.patch # a7R VI camera entry
        ];
    }
  );

  # libraw (used by darktable's "rawspeed off" path and most other RAW apps):
  # add the a7R VI camera (color matrix + camera list) and the reverse-engineered
  # lossy arw6 decoder in one patch. libraw builds via autotools, so the new
  # src/decoders/sonyarw6.cpp added to Makefile.am is picked up by autoreconfHook;
  # no extra build wiring is needed beyond the patch.
  libraw =
    if ! withLibRaw
    then prev.libraw
    else
      prev.libraw.overrideAttrs (old: {
        patches =
          (old.patches or [])
          ++ [
            ./patches/libraw-arw6.patch
          ];
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
