default:
  @just --list

# Run hoogle
docs:
  echo http://127.0.0.1:8888
  hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
  cabal repl {{ARGS}}

# Autoformat the project tree
fmt:
  treefmt

# run `main` function
run:
  cabal run exe:soltan

# Run ghcid -- auto-recompile and run tests
test:
  ghcid --command="cabal repl test:soltan-test" --test "Main.main"
  # ghcid --command="cabal repl --enable-multi-repl lib:soltan test:soltan-test" --test "Main.main"

