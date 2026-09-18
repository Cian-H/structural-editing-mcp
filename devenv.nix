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
}
