# USBeehive: a CLI and D-Bus daemon for USB hubs.
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
    # A module argument, not home.packages, so the profile places it in its own list.
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
