# Options home.nix reads and every profile must set. Kept apart so home.nix stays plain.
{ lib, ... }:
{
  options.dotfiles.repoPath = lib.mkOption {
    type = lib.types.str;
    example = "/home/you/dotfiles";
    description = ''
      Absolute path to this repository's checkout on this machine.

      Deployed dotfiles are out-of-store symlinks into the checkout, so edits
      take effect without a rebuild -- which is also why the path cannot be
      baked into the shared config: machines keep the repo in different places.
    '';
  };

  options.dotfiles.homeExcludes = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    example = [ ".bashrc" ];
    description = ''
      Repository-root dotfiles the blanket link should skip, because this
      profile deploys its own version of them instead.

      The case for this is a file that local tooling writes to: it has to be a
      file only that machine's branch carries, so the tooling's edits show up
      as a diff there rather than landing on a file shared with other machines.
    '';
  };

  options.dotfiles.configExcludes = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    example = [ "vale/some-extra" ];
    description = ''
      Extra paths under .config, relative to it, that this profile wants kept
      in the repo but out of ~/.config -- flake sources, or fragments spliced
      into a generated file rather than read from where they sit.

      Appended to the shared exclusions in home.nix, so a profile can add its
      own without editing them.
    '';
  };
}
