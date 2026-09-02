# Emacs as a user service, running the host system's Emacs rather than a
# nix-built one.
{ config, lib, pkgs, ... }:
let
  cfg = config.dotfiles.emacs;
in {
  options.dotfiles.emacs = {
    enable = lib.mkEnableOption "the Emacs daemon as a user service";

    version = lib.mkOption {
      type = lib.types.str;
      default = "30.2";
      description = ''
        The host Emacs's version. It cannot be read from /usr/bin/emacs while
        building -- the sandbox has no /usr -- so it is stated here and checked
        against the real binary on every switch, which complains if this has
        gone stale.

        It is not cosmetic: it lands in the package name, which is what
        lib.getVersion reads, and home-manager gates its pre-28 socket
        workaround (RefuseManualStart, chmod -w on the socket dir) on that. An
        unversioned name parses as "", compares as older than 28, and would
        saddle a modern Emacs with the workaround. Only the >=28 floor
        actually matters, not the exact figure.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.emacs = let
      # The name carries the version; see the option above.
      sysEmacs = pkgs.runCommand "system-emacs-${cfg.version}"
        { inherit (cfg) version; } ''
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

    # What the sandbox could not do at build time, done here instead: ask the
    # real binary. Only reports -- a wrong version is a latent misconfiguration,
    # not a reason to fail the switch.
    home.activation.checkSystemEmacsVersion =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        real=$(/usr/bin/emacs --version 2>/dev/null | sed -n '1s/^GNU Emacs //p')
        if [ -z "$real" ]; then
          echo "NOTE: could not read /usr/bin/emacs --version; dotfiles.emacs.version stays ${cfg.version}." >&2
        elif [ "$real" != "${cfg.version}" ]; then
          echo "NOTE: /usr/bin/emacs is $real, dotfiles.emacs.version says ${cfg.version} -- update it." >&2
          case "$real" in
            2[0-7].*|1?.*|[0-9].*)
              echo "WARNING: that is older than 28, so home-manager's socket workaround is being skipped when it should not be." >&2 ;;
          esac
        fi
      '';
  };
}
