final: prev: {
  deadbeef-with-plugins = prev.deadbeef-with-plugins.override {
    plugins = [final.deadbeef-mpris2-plugin];
  };
  wine = final.wineStaging;

  # Install the routing rules at priority 5280 instead of 0, so that Tailscale and other
  # WireGuard tunnels can be layered around Mullvad. Also stops Mullvad from deleting
  # identically-shaped rules belonging to other tools.
  mullvad = prev.mullvad.overrideAttrs (old: {
    patches = (old.patches or []) ++ [./0001-Set-base-rule-priority.patch];
  });

  # Sony ARW6 (lossy compressed RAW) decoding + Sony a7R VI (ILCE-7RM6) support.

  libraw = prev.libraw.overrideAttrs (old: {
    patches =
      (old.patches or [])
      ++ [
        # Sony ARW6 CRAW HQ decoder (merged to master).
        (final.fetchpatch {
          name = "libraw-sony-arw6-decoder.patch";
          url = "https://github.com/LibRaw/LibRaw/commit/f6b3a5008c04c624b739c0793dce6779ed43e792.diff";
          hash = "sha256-WZMi/RAap2IyiAq8W+nPs7bKD8hs5c7bjIzczsEQ3jA=";
        })
        # Adjust ARW6 black/white/linear_max points (merged to master).
        (final.fetchpatch {
          name = "libraw-arw6-levels.patch";
          url = "https://github.com/LibRaw/LibRaw/commit/e419de08001de28ae6988ecb22df47e52b9c5eaa.diff";
          hash = "sha256-cy/jnc2teuQVnpEg6HXKINH6vkL22N+0d+zm4d4iaP8=";
        })
      ];
  });

  dnglab = prev.dnglab.overrideAttrs (old: {
    patches =
      (old.patches or [])
      ++ [
        # Empty arw6 module scaffolding (upstream commit, post-0.7.2): adds the
        # arw6 module stub, Sony A7V/a7m5.toml and the arw.rs hooks that PR #825
        # builds on top of.
        (final.fetchpatch {
          name = "dnglab-arw6-module-scaffolding.patch";
          url = "https://github.com/dnglab/dnglab/commit/db5a1ac1b9b196a643ccae22c1affb4c3bf82f55.diff";
          hash = "sha256-S91tYwLdEJl/k1GjLQLCyGcjyWv9CtfmaRmtF37cClk=";
        })
        # arw6/mod.rs doc-comment header added by a later reorg commit; scoped to
        # just mod.rs so PR #825 (which expects this state) applies cleanly.
        (final.fetchpatch {
          name = "dnglab-arw6-mod-header.patch";
          url = "https://github.com/dnglab/dnglab/commit/a5e8e2e34b0cf40f78e09f86275f79b656ce791f.diff";
          includes = ["*arw6/mod.rs"];
          hash = "sha256-Y7qbkTyl0hh6QXmRekEYs0DiWr6eB8AKTSyHu+Pcn4Q=";
        })
        # Sony ARW6 decompressor + a7R VI (a7rm6.toml) support (PR #825).
        (final.fetchpatch {
          name = "dnglab-arw6-decompressor-pr825.patch";
          url = "https://github.com/dnglab/dnglab/pull/825.diff";
          hash = "sha256-IhkgBFriJQ+pSC0UwzIliPPQIrdDU5+sF+3NIQMH0zY=";
        })
      ];
  });

  darktable = prev.darktable.overrideAttrs (old: {
    patches =
      (old.patches or [])
      ++ [
        # rawspeed Sony ARW6 support (PR #983), applied to the rawspeed sources
        # bundled in the darktable release tarball.
        (final.fetchpatch {
          name = "rawspeed-sony-arw6-pr983.patch";
          url = "https://github.com/darktable-org/rawspeed/pull/983.diff";
          stripLen = 1;
          extraPrefix = "src/external/rawspeed/";
          hash = "sha256-yvF9OclHs7zAq1bkFXlXpLR5O7Zc7xQ2J2p9BH27q2I=";
        })
        # rawspeed Sony ILCE-7RM6 support (PR #979).
        (final.fetchpatch {
          name = "rawspeed-sony-ilce7rm6-pr979.patch";
          url = "https://github.com/darktable-org/rawspeed/pull/979.diff";
          stripLen = 1;
          extraPrefix = "src/external/rawspeed/";
          hash = "sha256-ZgSQCUY1rn+KBbDZx91Gw6D4xWIPBLLLXyQl1KZE/9k=";
        })
      ];
  });
}
