{pkgs, ...}: {
  packages = [
    (pkgs.sbcl.withPackages (ps:
      with ps; [
        trivia
        alexandria
        serapeum
        rove
        yason
      ]))
  ];
  enterShell = ''
    git config core.hooksPath .githooks 2>/dev/null || true
  '';
}
