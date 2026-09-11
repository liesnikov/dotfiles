# zotero-mcp (github.com/54yyyu/zotero-mcp), an MCP server over the local Zotero library.
{ lib
, python3Packages
, rustPlatform
, cargo
, rustc
  # Embedding-backed search. Off by default: it grows the closure 340MiB -> 1.2GiB.
, withSemantic ? false
}:

let
  # nixpkgs has 1.11.0, below the >=1.13.3 asked for. Only src moves: the deps match.
  # https://github.com/urschrei/pyzotero/issues/341
  pyzotero = python3Packages.pyzotero.overridePythonAttrs (_: rec {
    version = "1.14.0";
    src = python3Packages.fetchPypi {
      pname = "pyzotero";
      inherit version;
      hash = "sha256-fNszx6WSuGUCZg7R71GIavcqI/XUZ7cRKUUiBE9B14s=";
    };
    # 1.14.0 already widened the uv_build pin, so the nixpkgs substitution finds no match.
    postPatch = "";
  });

  # PDF text extraction. zotero-mcp pins it exactly while pre-1.0, so do not drift off it.
  pdf-inspector = python3Packages.buildPythonPackage rec {
    pname = "pdf-inspector";
    version = "0.2.6";
    pyproject = true;

    src = python3Packages.fetchPypi {
      pname = "pdf_inspector";
      inherit version;
      hash = "sha256-W7OH85v3qTsCtJGItnC5eY+MzH5Y9o7uWIOlEqoFzrI=";
    };

    cargoDeps = rustPlatform.fetchCargoVendor {
      inherit pname version src;
      hash = "sha256-/PTqpmL2JdnK/Ejo3IAK/DqTSVrA9zTmFnmRPoc4tLc=";
    };

    nativeBuildInputs = [
      rustPlatform.cargoSetupHook
      rustPlatform.maturinBuildHook
      cargo
      rustc
    ];

    # tounicode.rs reads CMaps at runtime, so keep them or CJK PDFs come out garbled.
    postInstall = ''
      mkdir -p $out/share/pdf-inspector
      cp -r external/bcmaps $out/share/pdf-inspector/bcmaps
    '';

    # The sdist ships no fixtures: the test corpus alone is over the crates.io 10 MiB cap.
    doCheck = false;
    pythonImportsCheck = [ "pdf_inspector" ];

    meta = {
      description = "Fast PDF inspection, classification, and text extraction";
      homepage = "https://github.com/firecrawl/pdf-inspector";
      license = lib.licenses.mit;
    };
  };

in
python3Packages.buildPythonApplication rec {
  pname = "zotero-mcp";
  version = "0.9.1";
  pyproject = true;

  src = python3Packages.fetchPypi {
    pname = "zotero_mcp_server";
    inherit version;
    hash = "sha256-tMstrPEZntmm/sK4xBWAuF8419rv6cOocEUxkcwKGNU=";
  };

  build-system = [ python3Packages.hatchling ];

  # `zotero-mcp update` would install into ~/.local and shadow this package.
  postPatch = ''
    # Cut at the installer, not the subcommand, so the version check still runs.
    substituteInPlace src/zotero_mcp/updater.py \
      --replace-fail 'package_name = "zotero-mcp-server"' \
        'return (False, "Self-update is disabled: this zotero-mcp comes from Nix. Bump the version and hash in .config/home-manager/packages/zotero-mcp/default.nix, then run: home-manager switch")'
    substituteInPlace src/zotero_mcp/cli.py \
      --replace-fail 'help="Update zotero-mcp to the latest version"' \
        'help="Check for a newer release (installing is disabled under Nix)"'
  '';

  # Half of the `semantic` extra. The rest is torch and hosted backends we do not use.
  dependencies = [
    pdf-inspector
    pyzotero
  ] ++ (with python3Packages; [
    bibtexparser
    fastmcp
    httpx
    markdownify
    pydantic
    python-dotenv
    requests
    unidecode
  ]) ++ lib.optionals withSemantic (with python3Packages; [
    chromadb
    tiktoken
  ]);

  # Default to the local library. --set-default keeps the web API env vars usable.
  makeWrapperArgs = [
    "--set-default"
    "ZOTERO_LOCAL"
    "true"
    "--set-default"
    "PDF_INSPECTOR_BCMAPS_DIR"
    "${pdf-inspector}/share/pdf-inspector/bcmaps"
  ];

  # Upstream's suite drives a live Zotero, so there is nothing to run in the sandbox.
  doCheck = false;
  pythonImportsCheck = [ "zotero_mcp" ];

  meta = {
    description = "MCP server bridging the local Zotero library to Claude";
    homepage = "https://github.com/54yyyu/zotero-mcp";
    license = lib.licenses.mit;
    mainProgram = "zotero-mcp";
  };
}
