{
  lib,
  config,
  ...
}:
with lib; {
  options = {
    home.relativeLinks = mkOption {
      type = types.attrsOf types.str;
      default = {};
      example = {".config/nvim" = ".config/home-manager/nvim";};
      description = "Create symbolic links relative to the home directory.";
    };
  };
  config = mkIf (config.home.relativeLinks != {}) {
    home.activation = {
      # TODO: remove the old symlinks
      createRelativeLinks = hm.dag.entryAfter ["writeBoundary"] (concatStringsSep "\n" (mapAttrsToList (from: to: ''
          targetPath="$HOME/"${escapeShellArg from}
          sourcePath="$HOME/"${escapeShellArg to}

          if [[ -e "$targetPath" && ! -L "$targetPath" && -n "$HOME_MANAGER_BACKUP_EXT" ]]; then
            # The target exists, back it up
            backup="$targetPath.$HOME_MANAGER_BACKUP_EXT"
            run mv $VERBOSE_ARG "$targetPath" "$backup" || errorEcho "Moving '$targetPath' failed!"
          fi
          run ln -Tsf $VERBOSE_ARG "$sourcePath" "$targetPath" || exit 1
        '')
        config.home.relativeLinks));
    };
  };
}
