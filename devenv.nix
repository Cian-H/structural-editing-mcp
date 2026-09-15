{pkgs, ...}: let
  customLisp = import ./nix/lisp-packages.nix {inherit pkgs;};
in {
  packages = [
    (pkgs.sbcl.withPackages (ps:
      with ps; [
        trivia
        rove
        customLisp.isocline-repl
      ]))
  ];
}
