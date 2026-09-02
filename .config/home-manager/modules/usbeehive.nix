# USBeehive: a CLI and D-Bus daemon for USBeehive-style USB hubs.
#
# Not in nixpkgs, so the derivation lives here. Importing this module makes the
# package available to a profile's home.packages as `usbeehive`; the daemon is
# separate, behind dotfiles.usbeehive.enable.
{ config, lib, pkgs, ... }:
let
  usbeehive = pkgs.rustPlatform.buildRustPackage rec {
    pname = "usbeehive";
    version = "0.11.0";
    src = pkgs.fetchFromGitHub {
      owner = "abrauchli";
      repo = "usbeehive";
      rev = "v${version}";
      hash = "sha256-5aqEqt0zwzG4O+roq0p4vs59z7s2ERPE+FzyW9waegw=";
    };
    cargoHash = "sha256-YX72/E1N59U6EU54SWpL8Ew/eMelAjnBF7xqpLYCNIo=";
    buildInputs = [ pkgs.udev ];
    nativeBuildInputs = [ pkgs.pkg-config ];
    buildNoDefaultFeatures = true;
    buildFeatures = [ "cli" "dbus" "sysfs" "watch" ];
  };
in {
  options.dotfiles.usbeehive.enable =
    lib.mkEnableOption "the USBeehived D-Bus daemon as a user service";

  config = {
    # Passed as a module argument rather than added to home.packages here, so a
    # profile decides where in its own list it sits.
    _module.args.usbeehive = usbeehive;

    systemd.user.services.usbeehived = lib.mkIf config.dotfiles.usbeehive.enable {
      Unit = {
        Description = "USBeehived D-Bus Daemon";
      };
      Service = {
        ExecStart = "${usbeehive}/bin/usbeehived";
        Restart = "on-failure";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
