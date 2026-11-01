{
  description = "A hunk-by-hunk code review TUI with built-in editing";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      remaxFor =
        pkgs:
        let
          # GHC 9.10, to match the stack resolver.
          haskellPackages = pkgs.haskell.packages.ghc910;
          remax = haskellPackages.callCabal2nix "remax" (pkgs.lib.cleanSource ./.) { };
        in
        (pkgs.haskell.lib.compose.justStaticExecutables remax).overrideAttrs (old: {
          # remax setup links skills from the checkout it was built in, which a
          # Nix build doesn't have. Ship them so a Nix user can link them.
          postInstall = (old.postInstall or "") + ''
            mkdir -p $out/share/remax
            cp -R ${./skills} $out/share/remax/skills
          '';
        });
    in
    {
      packages = forAllSystems (pkgs: rec {
        remax = remaxFor pkgs;
        default = remax;
      });

      overlays.default = final: prev: { remax = remaxFor final; };

      formatter = forAllSystems (pkgs: pkgs.nixfmt-tree);
    };
}
