{ config, pkgs, emacs-lsp-booster, lib, ... }:
  let
    repoRoot = ../..;

    # List files under dir, skipping paths (or prefixes) in exclude.
    walk = dir: exclude:
      let
        go = d: prefix:
          let entries = builtins.readDir d;
          in lib.concatMap
            (name:
              let rel = if prefix == "" then name else "${prefix}/${name}";
              in
              if lib.elem rel exclude then []
              else if entries.${name} == "directory" then go (d + "/${name}") rel
              else [ rel ])
            (builtins.attrNames entries);
      in go dir "";

    # Symlink every file under repoDir (minus exclude) from liveDir.
    linkTree = { link, repoDir, liveDir, exclude ? [], keyPrefix ? "" }:
      lib.listToAttrs (map
        (rel: lib.nameValuePair (if keyPrefix == "" then rel else "${keyPrefix}/${rel}")
          { source = link "${liveDir}/${rel}"; })
        (walk repoDir exclude));

  in {
    imports = [ ./options.nix ];

    # The repo's deployment helpers, for profiles and modules that link their own trees.
    _module.args.dotfilesLib = { inherit walk linkTree; };

    # home.username / home.homeDirectory come from the profile.

    home.sessionVariables = {
      QT_QPA_PLATFORMTHEME = "qgnomeplatform";
    };

    # Do not bump this on a Home Manager update.
    home.stateVersion = "25.05";

    targets.genericLinux.enable = true;
    fonts.fontconfig.enable = true;
    programs.home-manager.enable = true;

    # Symlink all desktop entries from the Nix profile into ~/.local/share/applications
    # so GNOME Shell and other desktop environments on non-NixOS discover them immediately.
    home.activation.linkDesktopApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      appsDir="$HOME/.local/share/applications"
      nixAppsDir="$HOME/.local/state/nix/profile/share/applications"
      mkdir -p "$appsDir"
      if [ -d "$nixAppsDir" ]; then
        for link in "$appsDir"/*.desktop; do
          if [ -L "$link" ] && ! [ -e "$link" ]; then
            target=$(readlink "$link" || true)
            case "$target" in
              *"$nixAppsDir"*|*"/nix/store"*)
                $DRY_RUN_CMD rm -f "$link"
                ;;
            esac
          fi
        done
        for file in "$nixAppsDir"/*.desktop; do
          if [ -f "$file" ]; then
            $DRY_RUN_CMD ln -sf "$file" "$appsDir/$(basename "$file")"
          fi
        done
        $DRY_RUN_CMD /usr/bin/update-desktop-database "$appsDir" 2>/dev/null || true
      fi
    '';

    home.file = linkTree {
      link = config.lib.file.mkOutOfStoreSymlink;
      repoDir = repoRoot;
      liveDir = config.dotfiles.repoPath;
      # not dotfiles to deploy: repo tooling, or (.config) handled separately
      exclude = [ ".config" ".git" ".claude" ".gitattributes" ".gitignore" "custom-settings.desktop" ]
        ++ config.dotfiles.homeExcludes;
    };

    xdg.configFile = let
      link = config.lib.file.mkOutOfStoreSymlink;
      dotfiles = "${config.dotfiles.repoPath}/.config";
    in
      linkTree {
        inherit link;
        repoDir = repoRoot + "/.config";
        liveDir = dotfiles;
        exclude = [
          "home-manager" # the flake itself
          "git" # not wired into home-manager yet
          # emacs runtime state
          "emacs/elpa" "emacs/eln-cache" "emacs/tree-sitter" "emacs/var"
          "emacs/transient" "emacs/auto-save-list" "emacs/.cache" "emacs/projects"
        ] ++ config.dotfiles.configExcludes;
      };

    dconf.settings = {
      "org/gnome/Console" = {
        restore-window-size = true;
        last-window-size = lib.hm.gvariant.mkTuple [ 1000 700 ];
      };
    };

    # Use the host system man, not the nix-provided one.
    programs.man.enable = false;
    home.extraOutputsToInstall = [ "man" ];

  }
