(import ./default.nix).shellFor {
  packages = ps: [ps.graphex];
  withHoogle = true;
}
