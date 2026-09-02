{
  description = "Home Manager configuration";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-lsp-booster = {
      url = "github:slotThe/emacs-lsp-booster-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, emacs-lsp-booster, ... }:
    let
      system = "x86_64-linux";
      lib = nixpkgs.lib;
      pkgs = nixpkgs.legacyPackages.${system};
      emacs-lsp-booster-pkg = emacs-lsp-booster.packages.${system}.default;

      # One output per name, from profiles/<name>.nix.
      profileNames = [ "bohdan" ];
    in {
      homeConfigurations = lib.genAttrs profileNames (name:
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          # home.nix holds everything shared; the profile supplies the
          # machine's identity and whatever only that machine wants.
          modules = [ ./home.nix (./profiles + "/${name}.nix") ];

          extraSpecialArgs = {
            emacs-lsp-booster = emacs-lsp-booster-pkg;
          };
        });
    };
}
