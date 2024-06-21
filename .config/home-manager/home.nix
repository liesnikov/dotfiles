{ config, pkgs, ... }:
  let
    custom-agda = pkgs.agda.withPackages [ pkgs.agdaPackages.standard-library ];
  in {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "bohdan";
    home.homeDirectory = "/home/bohdan";

    home.packages = with pkgs; [
      direnv
      nix-direnv
      xkb-switch

      vale

      nodejs

      haskellPackages.hasktags
      alex
      happy

      custom-agda

      pandoc
      ripgrep
      fd
      jq

      diffpdf
    ];
    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "24.05";

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # Don't use nix-provided man but rather host system one
    programs.man.enable = false;
    home.extraOutputsToInstall = [ "man" ];
  }
