{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-25.11-darwin";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Custom packages
        elm-watch = pkgs.callPackage (self + /nix/elm-watch.nix) { };
        run-pty = pkgs.callPackage (self + /nix/run-pty.nix) { };
        daisyui = pkgs.callPackage (self + /nix/daisyui.nix) { };
      in
      {
        packages = {
          build = pkgs.writeShellScriptBin "build" ''
            export PATH="${pkgs.elmPackages.elm}/bin:$PATH"

            # Setup node_modules for daisyui plugin
            mkdir -p node_modules
            ln -sfn ${daisyui} node_modules/daisyui

            ${pkgs.tailwindcss_4}/bin/tailwindcss -i ./src/input.css -o ./public/build/output.css --minify
            ${elm-watch}/bin/elm-watch make --optimize
            ${pkgs.esbuild}/bin/esbuild app.ts --bundle --outdir=public/build --public-path=/build/ --minify
          '';

          dev = pkgs.writeShellScriptBin "dev" ''
            export PATH="${pkgs.elmPackages.elm}/bin:$PATH"

            # Setup node_modules for daisyui plugin
            mkdir -p node_modules
            ln -sfn ${daisyui} node_modules/daisyui

            ${run-pty}/bin/run-pty % ${pkgs.tailwindcss_4}/bin/tailwindcss -i ./src/input.css -o ./public/build/output.css --watch % ${elm-watch}/bin/elm-watch hot % ${pkgs.esbuild}/bin/esbuild app.ts --bundle --outdir=public/build --public-path=/build/ --serve=9000 --servedir=public
          '';

          test = pkgs.writeShellScriptBin "test" ''
            export PATH="${pkgs.elmPackages.elm}/bin:$PATH"
            ${pkgs.elmPackages.elm-test}/bin/elm-test
          '';
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            esbuild
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-test
            tailwindcss_4
          ] ++ [
            elm-watch
            run-pty
          ];

          shellHook = ''
            echo "carp_dx - Dev Environment"
            echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            echo ""
            echo "Nix-managed tools:"
            echo "  • esbuild: $(esbuild --version)"
            echo "  • elm: $(elm --version)"
            echo "  • elm-watch: $(elm-watch --help | tail -1 | xargs)"
            echo "  • run-pty: $(run-pty --help | head -1)"
            echo ""
            echo "Commands:"
            echo "  • nix run .#build - Build for production"
            echo "  • nix run .#dev   - Start development server"
            echo "  • nix run .#test  - Run tests"
            echo ""
            echo "✅ All dependencies managed by Nix!"
            echo ""
          '';
        };
      });
}
