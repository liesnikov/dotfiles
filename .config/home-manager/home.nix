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

    # The repo's own deployment helpers, for profiles and for the modules
    # under ./modules that link trees of their own.
    _module.args.dotfilesLib = { inherit walk linkTree; };

    # home.username / home.homeDirectory come from the profile.

    home.sessionVariables = {
      QT_QPA_PLATFORMTHEME = "qgnomeplatform";
    };

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "25.05";

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

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

    # Don't use nix-provided man but rather host system one
    programs.man.enable = false;
    home.extraOutputsToInstall = [ "man" ];

  }
