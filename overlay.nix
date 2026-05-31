final: prev: {
  deadbeef-with-plugins = prev.deadbeef-with-plugins.override {
    plugins = [final.deadbeef-mpris2-plugin];
  };
  wine = final.wineStaging;

  # Sony a7R VI (ILCE-7RM6) lossless/uncompressed ARW support.
  # darktable: patch its vendored rawspeed (cameras.xml entry + raised
  # ArwDecoder LJPEG dimension guard for the 10240x7168 frame).
  # dnglab: add rawler's a7rm6.toml so `dnglab convert` reads the ARW.
  # The lossy "Sony Compressed RAW 2" (compression 32766) is not covered.
  darktable = prev.darktable.overrideAttrs (old: {
    patches =
      old.patches
      or []
      ++ [
        ./patches/darktable-rawspeed-a7rm6.patch
      ];
  });
  dnglab = prev.dnglab.overrideAttrs (old: {
    patches =
      old.patches
      or []
      ++ [
        ./patches/dnglab-a7rm6.patch
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
