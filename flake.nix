{
  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay/master";
  };

  outputs = { self, emacs-overlay }: {
    nixosModules.common = import ./configuration-common.nix;
    nixosModules.workplace.imports = [
      ./configuration-workplace.nix
      { nixpkgs.overlays = [ emacs-overlay.overlays.default ]; }
    ];
    nixosModules.no-graphics = import ./configuration-no-graphics.nix;
  };
}
