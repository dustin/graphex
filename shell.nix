(import ./default.nix).shellFor {
  tools = {
    cabal = "latest";
    hsc2hs = "latest";
    stylish-haskell = "latest";
  };

  withHoogle = false;
}
