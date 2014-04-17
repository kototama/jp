# Experiment with Haskell and JSON.

Usage example:

`
source ./scripts/sandbox.sh
cabal build && echo '{"a": 10, "b": 20}' | sandbox exec ./dist/build/jp/jp ' <&> members . _Number *~ 10  '
`
