# zotero-mcp (github.com/54yyyu/zotero-mcp), an MCP server over the local
# Zotero library. Packaged from its PyPI distribution `zotero-mcp-server`: the
# name `zotero-mcp` there belongs to an unrelated project (kujenga/zotero-mcp),
# so upstream publishes under the longer name while keeping `zotero-mcp` as the
# command and `zotero_mcp` as the module.
{ lib
, python3Packages
, rustPlatform
, cargo
, rustc
  # Embedding-backed search. Off by default: it grows the closure 340MiB -> 1.2GiB.
, withSemantic ? false
}:

let
  # nixpkgs 26.05 has 1.11.0, below the >=1.13.3 zotero-mcp asks for: 1.13.3
  # is where upload step 0 started sending Path(filename).name rather than the
  # caller's whole path (urschrei/pyzotero#341), which the API rejects with a
  # 400 that pyzotero reports under `failure` instead of raising. Dependencies
  # are unchanged between the two versions, so only src moves.
  pyzotero = python3Packages.pyzotero.overridePythonAttrs (_: rec {
    version = "1.14.0";
    src = python3Packages.fetchPypi {
      pname = "pyzotero";
      inherit version;
      hash = "sha256-fNszx6WSuGUCZg7R71GIavcqI/XUZ7cRKUUiBE9B14s=";
    };
    # The nixpkgs build rewrites a "uv_build>=0.8.14,<0.9.0" build-system pin
    # that 1.14.0 has already widened to <0.12.0 -- satisfied by the uv-build
    # in nixpkgs -- so the substitution now finds no match and fails the build.
    postPatch = "";
  });

  # PDF text extraction: PyO3 bindings over Firecrawl's Rust library, not in
  # nixpkgs. zotero-mcp pins it exactly (==0.2.6) while it is pre-1.0 because
  # the extraction output is user-visible, so don't drift off that pin.
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

    # tounicode.rs reads pdf.js's binary CMaps off disk at runtime, defaulting
    # to CARGO_MANIFEST_DIR -- the build sandbox, long gone by then. Keep them
    # in the output and point PDF_INSPECTOR_BCMAPS_DIR (set on the wrapper
    # below) at them, so CJK CMap PDFs still extract. Without it the lookup
    # returns nothing and only those PDFs come out garbled.
    postInstall = ''
      mkdir -p $out/share/pdf-inspector
      cp -r external/bcmaps $out/share/pdf-inspector/bcmaps
    '';

    # The sdist ships no fixtures: crates.io caps uploads at 10 MiB and the
    # test corpus alone is over it, so upstream excludes tests/ entirely.
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

  # `zotero-mcp update` shells out to pip/uv to overwrite its own install,
  # which here either fails against the read-only store or, worse, succeeds
  # into ~/.local and shadows this package with a copy Nix no longer tracks.
  # Cut it at the one function that runs the installer rather than at the
  # subcommand: the version check above it still works -- that is how a due
  # bump gets noticed -- and the refusal travels back through upstream's own
  # (success, message) path, so `update` reports it and exits cleanly. The rest
  # of update_via_method is unreachable by design. --replace-fail means a
  # reworded upstream breaks the build instead of quietly restoring
  # self-update.
  postPatch = ''
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

  # Default to the local library, so the wrapped `zotero-mcp` and `zotero-cli`
  # talk to the running Zotero without per-client env plumbing. --set-default
  # keeps ZOTERO_API_KEY/ZOTERO_LIBRARY_ID usable for the web API instead.
  makeWrapperArgs = [
    "--set-default"
    "ZOTERO_LOCAL"
    "true"
    "--set-default"
    "PDF_INSPECTOR_BCMAPS_DIR"
    "${pdf-inspector}/share/pdf-inspector/bcmaps"
  ];

  # Upstream's suite drives a live Zotero on port 23119; nothing to run in the
  # sandbox.
  doCheck = false;
  pythonImportsCheck = [ "zotero_mcp" ];

  meta = {
    description = "MCP server bridging the local Zotero library to Claude";
    homepage = "https://github.com/54yyyu/zotero-mcp";
    license = lib.licenses.mit;
    mainProgram = "zotero-mcp";
  };
}
