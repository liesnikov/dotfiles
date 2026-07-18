{ config, pkgs, emacs-lsp-booster, nixgl, ... }:
  let
    custom-agda = pkgs.agda.withPackages
      [ pkgs.agdaPackages.standard-library ];

    ghostty-nixgl = pkgs.symlinkJoin {
      name = "ghostty-nixgl";
      paths = [ pkgs.ghostty ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        rm $out/bin/ghostty
        makeWrapper ${nixgl}/bin/nixGLIntel $out/bin/ghostty \
          --add-flags "${pkgs.ghostty}/bin/ghostty"
      '';
    };
  in {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    home.username = "bohdan";
    home.homeDirectory = "/home/bohdan";

    home.packages = with pkgs; [

      direnv
      nix-direnv # support direnv for flakes
      nixfmt-rfc-style # format nix files
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

      bash-language-server

      ghostty-nixgl

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

    # Don't use nix-provided man but rather host system one
    programs.man.enable = false;
    home.extraOutputsToInstall = [ "man" ];
  }
