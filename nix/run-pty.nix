{ lib
, buildNpmPackage
, fetchFromGitHub
}:

buildNpmPackage rec {
  pname = "run-pty";
  version = "5.0.0";

  src = fetchFromGitHub {
    owner = "lydell";
    repo = "run-pty";
    rev = "v${version}";
    hash = "sha256-Ctgtx1ju2w/T1/raDFaBwb/AsxGjZNvqb+loclgVg54=";
  };

  npmDepsHash = "sha256-NpPNfmHxh4OxztcpQQh79Ms6i2MrCs7qROER5FEtRco=";

  # run-pty doesn't need a build step
  dontNpmBuild = true;

  meta = with lib; {
    description = "Run several commands concurrently. Show output for one command at a time. Kill all at once.";
    homepage = "https://github.com/lydell/run-pty";
    license = licenses.mit;
    mainProgram = "run-pty";
  };
}
