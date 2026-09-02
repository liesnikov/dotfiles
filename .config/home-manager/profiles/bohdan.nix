# Personal machine.
#
# Anything here is deliberately *not* in home.nix: it is wanted on this machine
# only, so another profile does not have to delete it. Shared configuration
# lives in ../home.nix; see ../options.nix for what a profile must set.
{ config, pkgs, lib, emacs-lsp-booster, usbeehive, dotfilesLib, ... }:
let
  custom-agda = pkgs.agda.withPackages
    [ pkgs.agdaPackages.standard-library ];

  lastfm-to-sqlite = pkgs.python3Packages.buildPythonApplication rec {
    pname = "lastfm-to-sqlite";
    version = "0.2.3";

    src = pkgs.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "04i8r3m3vkg0s1inddhalvvl1zwlzl8506v81kdl2lf10fw3jmrl";
    };

    pyproject = true;

    build-system = with pkgs.python3Packages; [
      setuptools
    ];

    propagatedBuildInputs = with pkgs.python3Packages; [
      pylast
      sqlite-utils
      click
      python-dateutil
      requests
    ];

    doCheck = false;
  };
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

    (symlinkJoin {
      name = "kid3-cli-wrapped";
      paths = [ kid3-cli ];
      buildInputs = [ makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/kid3-cli \
          --prefix GST_PLUGIN_SYSTEM_PATH_1_0 : "${lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" (with gst_all_1; [ gstreamer gst-plugins-base gst-plugins-good ])}"
      '';
    })
    beets
    lastfm-to-sqlite
    puddletag

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
      ulem         # org puts it in every preamble it builds, previews included
    ]))
  ];
}
