{
  inputs = {
  };

  outputs = { self }: {
    nixosModules.common = import ./configuration-common.nix;
    nixosModules.workplace.imports = [
      ./configuration-workplace.nix
    ];
    nixosModules.no-graphics = import ./configuration-no-graphics.nix;
  };
}
