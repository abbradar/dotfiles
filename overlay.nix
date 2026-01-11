final: prev: {
  deadbeef-with-plugins = prev.deadbeef-with-plugins.override {
    plugins = [final.deadbeef-mpris2-plugin];
  };
  wine = final.wineStaging;
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
