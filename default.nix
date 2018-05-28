# To pin to a specific version of nixpkgs, you can substitute <nixpkgs> with:
# `(builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/<nixpkgs_commit_hash>.tar.gz")`
{ compiler ? "ghc841"
, pkgs     ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/d3f070fa995756ef5fdf866324f86cfa0472a5d3.tar.gz") {} }:
  pkgs.haskell.packages.${compiler}.developPackage {
    root = ./.;
    overrides = self: super: with pkgs.haskell.lib; {
      # Don't run a package's test suite
      heist = dontCheck (doJailbreak super.heist);
      threads = dontCheck (doJailbreak super.threads);
      postgresql-simple = dontCheck (doJailbreak super.postgresql-simple);

      # Don't enforce package's version constraints
      # bar = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.bar;
      #
      # To discover more functions that can be used to modify haskell
      # packages, run "nix-repl", type "pkgs.haskell.lib.", then hit
      # <TAB> to get a tab-completed list of functions.
    };
    source-overrides = {
      # Use a specific hackage version
      # postgresql-simple = "0.5.4.0";
      # snap = "1.1.1.0";

      # Use a particular commit from github
      heist = pkgs.fetchFromGitHub
        { owner = "snapframework";
          repo = "heist";
          rev = "3ccbec548830abce7ed7eba42c1c294b02b6cd52";
          sha256 = "14sd4d4an7fj8yb4mr8cdallsv69x5jb1hd330sg10ahi1ryzspr";
        };
      io-streams-haproxy = pkgs.fetchFromGitHub
        { owner = "snapframework";
          repo = "io-streams-haproxy";
          rev = "a273d8873aadb3e84723be04df4de03fa3b27588";
          sha256 = "0b8k29i23101mzakbfx1qvc760ihiv2igsfi0nyr4lq11qbc80ps";
        };
      map-syntax = pkgs.fetchFromGitHub
        { owner = "mightybyte";
          repo = "map-syntax";
          rev = "acebcf0a83ee639e1a0c49850b9c85821d53f621";
          sha256 = "076knpvls1489gish9z30lhb21vqx44k366vc2i3kdql815v1vqv";
        };
      postgresql-libpq = pkgs.fetchFromGitHub
        { owner = "lpsmith";
          repo = "postgresql-libpq";
          rev = "2a401d5047ecbe89a200a9fdd421f8b51e4237d3";
          sha256 = "11hxcdshic61w61xljpc4ls49d0ibyknbl0fripwxls456yawffm";
        };
      postgresql-simple = pkgs.fetchFromGitHub
        { owner = "hackage-trustees";
          repo = "postgresql-simple";
          rev = "0ff20911d647a27e9bd9d31bc06a5c62d882321c";
          sha256 = "0kln85b6brllf5mb5mkyid18ps354ycksjqlcghxw2dbx470sz5h";
        };
      snap = pkgs.fetchFromGitHub
        { owner = "snapframework";
          repo = "snap";
          rev = "2fa933b52d7d126b59d89eddeed0e8a9d58d1d61";
          sha256 = "002byv0iqmxj60c1q8ybnipvaqsjy7j7hv8rd7drdbc2cz422wlh";
        };
      snap-server = pkgs.fetchFromGitHub
        { owner = "snapframework";
          repo = "snap-server";
          rev = "deac24c293b910f253c273258484928891d2152e";
          sha256 = "1bgknkiv6l2k4skja5q8nprdc1csawz85rjmvvzvmb23zr5gza8k";
        };
    };
  }
