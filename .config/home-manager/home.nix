{ config, pkgs, emacs-lsp-booster, lib, ... }:
  let
    custom-agda = pkgs.agda.withPackages
      [ pkgs.agdaPackages.standard-library ];
  in {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "bohdan";
    home.homeDirectory = "/home/bohdan";

    home.packages = with pkgs; [

      direnv
      nix-direnv # support direnv for flakes
      nixfmt # format nix files
      # nix-index # tool to find nixpkgs fields corresponding to paths
      nixd # language server for nix

      cachix # for better nix caches

      vale # for writing
      vale-ls # lsp server for vale
      texlab # lsp for latex

      nodejs # for github copilot
      emacs-lsp-booster # for lsp-booster

      haskellPackages.hasktags
      alex
      happy

      custom-agda
      #agda

      pandoc
      librsvg # for rsvg-convert

      ripgrep
      fd
      jq

      diffpdf
      pdfsandwich
      tesseract

      tmux

      bash-language-server

      # TeX for AUCTeX preview-latex / texfrag inline math previews
      # (system ghostscript at /usr/bin/gs handles the PDF -> image step).
      (texlive.withPackages (ps: with ps; [
        scheme-basic # pdflatex and the core engine
        preview      # preview.sty
        mylatex      # mylatex.ltx, for preview-latex's preamble cache
        dvipng       # DVI -> PNG
        dvisvgm      # DVI/PDF -> SVG
        amsmath      # align, gather, ...
        amsfonts     # amssymb and friends
      ]))
    ];
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

    home.file = let
      link = config.lib.file.mkOutOfStoreSymlink;
      dotfiles = "${config.home.homeDirectory}/dotfiles";
    in {
      ".bash_aliases".source = link "${dotfiles}/.bash_aliases";
      ".bash_functions".source = link "${dotfiles}/.bash_functions";
      ".bash_logout".source = link "${dotfiles}/.bash_logout";
      ".bashrc".source = link "${dotfiles}/.bashrc";
      ".profile".source = link "${dotfiles}/.profile";
      ".local/bin/caps-unlock".source = link "${dotfiles}/.local/bin/caps-unlock";
      ".local/bin/xfce-toggle-cycle-workspaces".source = link "${dotfiles}/.local/bin/xfce-toggle-cycle-workspaces";
    };

    xdg.configFile = let
      link = config.lib.file.mkOutOfStoreSymlink;
      dotfiles = "${config.home.homeDirectory}/dotfiles/.config";
    in {
      "alacritty/alacritty.toml".source = link "${dotfiles}/alacritty/alacritty.toml";
      "emacs/.gitignore".source = link "${dotfiles}/emacs/.gitignore";
      "emacs/custom.el".source = link "${dotfiles}/emacs/custom.el";
      "emacs/early-init.el".source = link "${dotfiles}/emacs/early-init.el";
      "emacs/init.el".source = link "${dotfiles}/emacs/init.el";
      "emacs/yasnippets/markdown-mode/footnote".source = link "${dotfiles}/emacs/yasnippets/markdown-mode/footnote";
      "emacs/yasnippets/markdown-mode/https".source = link "${dotfiles}/emacs/yasnippets/markdown-mode/https";
      "ghostty/config.ghostty".source = link "${dotfiles}/ghostty/config.ghostty";
      "gtk-3.0/gtk.css".source = link "${dotfiles}/gtk-3.0/gtk.css";
      "nix/nix.conf".source = link "${dotfiles}/nix/nix.conf";
      "python/pythonrc".source = link "${dotfiles}/python/pythonrc";
      "tmux/tmux.conf".source = link "${dotfiles}/tmux/tmux.conf";
      "vale/.vale.ini".source = link "${dotfiles}/vale/casual.ini";
      "vale/academic.ini".source = link "${dotfiles}/vale/academic.ini";
      "vale/casual.ini".source = link "${dotfiles}/vale/casual.ini";
      "vale/config/vocabularies/type-theory/accept.txt".source = link "${dotfiles}/vale/config/vocabularies/type-theory/accept.txt";
      "vale/config/vocabularies/type-theory/reject.txt".source = link "${dotfiles}/vale/config/vocabularies/type-theory/reject.txt";
      "vim/ftplugin/c.vim".source = link "${dotfiles}/vim/ftplugin/c.vim";
      "vim/ftplugin/cpp.vim".source = link "${dotfiles}/vim/ftplugin/cpp.vim";
      "vim/ftplugin/cs.vim".source = link "${dotfiles}/vim/ftplugin/cs.vim";
      "vim/ftplugin/haskell.vim".source = link "${dotfiles}/vim/ftplugin/haskell.vim";
      "vim/ftplugin/python.vim".source = link "${dotfiles}/vim/ftplugin/python.vim";
      "vim/vimrc".source = link "${dotfiles}/vim/vimrc";
      "xdg-terminals.list".source = link "${dotfiles}/xdg-terminals.list";
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
