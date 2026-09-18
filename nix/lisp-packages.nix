{pkgs}: let
  styled-strings-src = pkgs.fetchFromGitea {
    domain = "codeberg.org";
    owner = "digikar";
    repo = "styled-strings";
    rev = "main";
    hash = "sha256-TIME9Z0eO7GFP8DFh/cvoYA/3O0SpEpsRqsiGJQZa0g=";
  };
  styled-strings = pkgs.sbcl.buildASDFSystem {
    pname = "styled-strings";
    version = "unstable";
    src = styled-strings-src;
    lispLibs = [
      pkgs.sbclPackages.alexandria
    ];
  };
  isocline-src = pkgs.fetchFromGitHub {
    owner = "digikar99";
    repo = "cl-isocline";
    rev = "master";
    hash = "sha256-T+3QeYGCq5pqvtJix5iLciDZhuK9zwec4ZFQHToqIss=";
  };
  isocline-c = pkgs.stdenv.mkDerivation {
    pname = "isocline-c";
    version = "1.0.9";
    src = isocline-src;
    buildPhase = ''
      gcc -shared -o libisocline.so -Iisocline/include -fpic isocline/src/isocline.c
    '';
    installPhase = ''
      mkdir -p $out/lib
      cp libisocline.so $out/lib/
    '';
  };
  isocline = pkgs.sbcl.buildASDFSystem {
    pname = "isocline";
    version = "1.0.9";
    src = isocline-src;
    lispLibs = with pkgs.sbclPackages; [cffi];
    postPatch = ''
      sed -i 's/(uiop:run-program/#+nil(uiop:run-program/gi' lisp/shared-object.lisp
      sed -i 's|libisocline\.so|${isocline-c}/lib/libisocline.so|g' lisp/shared-object.lisp
    '';
  };
  isocline-repl = pkgs.sbcl.buildASDFSystem {
    pname = "isocline-repl";
    version = "1.0.9";
    src = isocline-src;
    lispLibs = [
      isocline
      styled-strings
      pkgs.sbclPackages.eclector-concrete-syntax-tree
    ];
  };
in {
  inherit isocline-repl styled-strings;
}
