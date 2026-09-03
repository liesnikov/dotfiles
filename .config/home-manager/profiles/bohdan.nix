# Personal machine. Only what this machine wants. Shared config is in ../home.nix.
{ config, pkgs, lib, emacs-lsp-booster, usbeehive, dotfilesLib, ... }:
let
  custom-agda = pkgs.agda.withPackages
    [ pkgs.agdaPackages.standard-library ];

in {
  imports = [
    ../modules/usbeehive.nix
    ../modules/vale.nix
    ../modules/emacs.nix
  ];

  dotfiles.usbeehive.enable = true;
  dotfiles.emacs.enable = true;

  dotfiles.vale = {
    enable = true;
    configs = {
      casual = [ ];
      academic = [ ../../vale/academic-extra ];
    };
    defaultConfig = "casual";
  };
  # Spliced into the generated vale/*.ini above, not read from where it sits.
  dotfiles.configExcludes = [ "vale/academic-extra" ];

  home.username = "bohdan";
  home.homeDirectory = "/home/bohdan";
  dotfiles.repoPath = "${config.home.homeDirectory}/dotfiles";

  home.packages = with pkgs; [

    direnv
    nix-direnv # support direnv for flakes
    angrr # auto nix gc root retention
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

    mcp-language-server

    custom-agda
    #agda
    usbeehive

    pandoc
    librsvg # for rsvg-convert

    ripgrep
    fd
    jq

    puddletag

    diffpdf
    pdfsandwich
    tesseract

    tmux

    bash-language-server

    # TeX for AUCTeX preview-latex / texfrag. System ghostscript does PDF -> image.
    (texlive.withPackages (ps: with ps; [
      scheme-basic # pdflatex and the core engine
      preview      # preview.sty
      mylatex      # mylatex.ltx, for preview-latex's preamble cache
      dvipng       # DVI -> PNG
      dvisvgm      # DVI/PDF -> SVG
      amsmath      # align, gather, ...
      amsfonts     # amssymb and friends
      ulem         # org puts it in every preamble it builds, previews included
    ]))
  ];
}
