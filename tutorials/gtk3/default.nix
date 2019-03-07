{ mkDerivation, base, gtk3, stdenv, transformers }:
mkDerivation {
  pname = "calc";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base gtk3 transformers ];
  homepage = "https://github.com/stackbuilders/tutorials";
  description = "The calculator is an example of a GUI application";
  license = stdenv.lib.licenses.mit;
}
