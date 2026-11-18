let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-25.05.tar.gz";
    sha256 = "0v6bd1xk8a2aal83karlvc853x44dg1n4nk08jg3dajqyy0s98np";
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
