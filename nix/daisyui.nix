{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  pname = "daisyui";
  version = "5.5.18";

  src = fetchurl {
    url = "https://registry.npmjs.org/daisyui/-/daisyui-${version}.tgz";
    sha256 = "4YpQBK9NmUA8PDRN2h9cBB5sjdmwhrwoUonVXlGogbs=";
  };

  dontBuild = true;

  installPhase = ''
    mkdir -p $out
    cp -r * $out/
  '';

  meta = {
    description = "daisyUI - Tailwind CSS Component Library";
    homepage = "https://daisyui.com";
  };
}
