# Emacs as a user service, running via Nixpkgs.
{ config, lib, pkgs, ... }:
let
  cfg = config.dotfiles.emacs;
in {
  options.dotfiles.emacs = {
    enable = lib.mkEnableOption "the Emacs daemon as a user service";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.emacs31-pgtk;
      description = "The Emacs package to install and run.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    services.emacs = {
      enable = true;
      package = cfg.package;
      # Start after the compositor exports WAYLAND_DISPLAY, or -c frames fail.
      startWithUserSession = "graphical";
      # Give the socket to systemd, so emacsclient starts this service instead of forking.
      socketActivation.enable = true;
      # client.enable stays off: we have our own emacsclient desktop entries.
    };

    # emacs-pgtk crashes on frame close when DISPLAY is set (stray GDK X11 seat).
    systemd.user.services.emacs.Service = {
      Environment = [ "GDK_BACKEND=wayland" ];
      UnsetEnvironment = [ "DISPLAY" ];
      ExecStart = lib.mkForce "${pkgs.bash}/bin/bash -l -c \"${config.services.emacs.package}/bin/emacs --fg-daemon\"";
    };
  };
}
