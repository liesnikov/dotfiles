# Emacs as a user service, running the host system's Emacs, not a nix-built one.
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
      # Start after the compositor exports WAYLAND_DISPLAY, or -c frames fail.
      startWithUserSession = "graphical";
      # Give the socket to systemd, so emacsclient starts this service instead of forking.
      socketActivation.enable = true;
      # client.enable stays off: we have our own emacsclient desktop entries.
    };

    # emacs-pgtk 30.2 crashes on frame close when DISPLAY is set (stray GDK X11 seat).
    systemd.user.services.emacs.Service = {
      Environment = [ "GDK_BACKEND=wayland" ];
      UnsetEnvironment = [ "DISPLAY" ];
      ExecStart = lib.mkForce "${pkgs.bash}/bin/bash -l -c \"${config.services.emacs.package}/bin/emacs --fg-daemon\"";
    };

    # Ask the real binary, which the build sandbox cannot. Reports only, never fails.
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
