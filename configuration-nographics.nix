{ lib, ... }:

{
  imports = [ ./configuration-common.nix ];

  programs.ssh.setXAuthLocation = false;
  security.pam.services.su.forwardXAuth = lib.mkForce false;

  fonts.fontconfig.enable = false;
}
