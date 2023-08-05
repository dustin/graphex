(import ./default.nix).shellFor {
  tools = {
    cabal = "latest";
    hsc2hs = "latest";
  };

  withHoogle = false;
}
