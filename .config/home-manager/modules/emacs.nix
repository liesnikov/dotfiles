# Emacs as a user service, running the host system's Emacs rather than a
# nix-built one.
{ config, lib, pkgs, ... }:
{
  options.dotfiles.emacs.enable =
    lib.mkEnableOption "the Emacs daemon as a user service";

  config = lib.mkIf config.dotfiles.emacs.enable {
    services.emacs = let
      # The version in the name is what lib.getVersion reads, and the module
      # gates its pre-28 socket workaround (RefuseManualStart, chmod -w on the
      # socket dir) on it. An unversioned name parses as "", which compares as
      # older than 28 and would saddle Emacs 30 with that workaround. Only the
      # >=28 floor matters here, not the exact figure.
      sysEmacs = pkgs.runCommand "system-emacs-30.2" { version = "30.2"; } ''
        mkdir -p $out/bin
        ln -s /usr/bin/emacs $out/bin/emacs
        ln -s /usr/bin/emacsclient $out/bin/emacsclient
      '';
    in {
      enable = true;
      package = sysEmacs;
      # client.enable stays off — we already have our own emacsclient
      # desktop entries and don't want the module's duplicate.
      # Start only once the Wayland compositor is up and has imported
      # WAYLAND_DISPLAY etc. into the systemd user environment. Otherwise the
      # daemon races ahead with no display and emacsclient -c frames fail.
      startWithUserSession = "graphical";
      # Hand /run/user/*/emacs/server to systemd, so an emacsclient that would
      # otherwise fall back to --alternate-editor= and fork its own daemon
      # outside the unit instead starts this service on connect.
      socketActivation.enable = true;
    };

    # emacs-pgtk 30.2 crashes on graphical-frame teardown under Wayland with
    #   gdk_wayland_seat_get_wl_seat: assertion 'GDK_IS_WAYLAND_SEAT (seat)' failed
    # whenever DISPLAY is also set: GDK builds a stray X11 seat via Xwayland and
    # trips the assertion when a frame is closed, killing the whole daemon. Pin
    # the GTK backend to Wayland and drop DISPLAY so the daemon stays alive
    # across emacsclient -c open/close cycles. (Keys merge into the module's
    # Service section.)
    systemd.user.services.emacs.Service = {
      Environment = [ "GDK_BACKEND=wayland" ];
      UnsetEnvironment = [ "DISPLAY" ];
      ExecStart = lib.mkForce "${pkgs.bash}/bin/bash -l -c \"${config.services.emacs.package}/bin/emacs --fg-daemon\"";
    };
  };
}
