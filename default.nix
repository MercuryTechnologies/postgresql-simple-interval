let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-25.05.tar.gz";
    sha256 = "1avlwbghhyzxqv72f5rh7bwbfkzyf237w7hlw119dlf7jgc7x3f1";
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
      pkgs.pinact
      pkgs.pkg-config
      pkgs.postgresql
      pkgs.zlib
    ];
}
