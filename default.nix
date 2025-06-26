let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-25.05.tar.gz";
    sha256 = "0854a0169bh2rlfvrsibaqlmsql0dp3ycwq5z8178kdl7q9h6rrq";
  }) { };
  hs = pkgs.haskell;
  ghc = hs.packages.ghc98;
in
ghc.developPackage {
  root = ./.;
  modifier =
    drv:
    hs.lib.addBuildTools drv [
      ghc.cabal-gild
      ghc.cabal-install
      ghc.haskell-language-server
      ghc.hlint
      ghc.ormolu
      pkgs.nixd
      pkgs.nixfmt-rfc-style
      pkgs.pkg-config
      pkgs.postgresql
    ];
}
