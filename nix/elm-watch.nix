{ lib
, buildNpmPackage
, fetchFromGitHub
}:

buildNpmPackage rec {
  pname = "elm-watch";
  version = "1.2.3";

  src = fetchFromGitHub {
    owner = "lydell";
    repo = "elm-watch";
    rev = "v${version}";
    hash = "sha256-CBh8Ag7/+xlSOnYa/53Vjz+P+cjG6Eyge5YQZT0dbc0=";
  };

  npmDepsHash = "sha256-jEAwVAE6oTGZNXeorW9pWxy2h80reWCq/9QmLHqgkvk=";

  # Skip postinstall script `elm-tooling install`
  # (elm is provided by nixpkgs instead)
  npmFlags = [ "--ignore-scripts" ];

  # elm-watch builds to build/ directory which contains the actual package
  installPhase = ''
    runHook preInstall

    # Install from the build directory
    mkdir -p $out/lib/node_modules/elm-watch
    cp -r build/* $out/lib/node_modules/elm-watch/

    # Copy production dependencies
    cp -r node_modules $out/lib/node_modules/elm-watch/

    # Create bin wrapper
    mkdir -p $out/bin
    ln -s $out/lib/node_modules/elm-watch/index.js $out/bin/elm-watch

    runHook postInstall
  '';

  meta = with lib; {
    description = "elm make in watch mode. Fast and reliable.";
    homepage = "https://github.com/lydell/elm-watch";
    license = licenses.mit;
    mainProgram = "elm-watch";
  };
}
