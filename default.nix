{ mkDerivation, base, haskell-src-meta, parsec, reflex, reflex-dom
, stdenv, template-haskell
}:
mkDerivation {
  pname = "reflex-jsx";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base haskell-src-meta parsec reflex reflex-dom template-haskell
  ];
  homepage = "https://github.com/dackerman/reflex-jsx";
  description = "Use jsx-like syntax in Reflex";
  license = stdenv.lib.licenses.bsd3;
}
