{pkgs, ...}: {
  packages = [
    (pkgs.sbcl.withPackages (ps:
      with ps; [
        alexandria
        cl-indentify
        rove
        serapeum
        trivia
        yason
      ]))
  ];
  enterShell = ''
    git config core.hooksPath .githooks 2>/dev/null || true
  '';
}
