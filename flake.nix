{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:abbradar/nixpkgs/stable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ {
    flake-parts,
    home-manager,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      flake = {
        nixosModules = {
          common = ./configuration-common.nix;
          workplace = ./configuration-workplace.nix;
          no-graphics = ./configuration-no-graphics.nix;
        };
      };
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: {
        formatter = pkgs.alejandra;
        legacyPackages.homeConfigurations = {
          namiantov = home-manager.lib.homeManagerConfiguration {
            inherit pkgs;
            modules = [
              ./home.nix
              ({lib, ...}:
                with lib; {
                  home.username = "namiantov";
                  home.homeDirectory = "/home/namiantov";
                  dconf.settings = mkForce {};
                })
            ];
          };
        };
      };
    };
}
