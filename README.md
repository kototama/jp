# JSON command-line utility in Haskell

## Usage

`
source ./scripts/sandbox.sh
cat ./tests/simple.json | sandbox exec ./dist/build/jp/jp -p 
sandbox exec ./dist/build/jp/jp ./tests/obj1.json -e '<&> members . _Number *~ 10'
`

## Pretty printing

## Lens expression

[Lens](https://github.com/ekmett/lens) expressions can be used to filter and modify the input.

Lens tutorials:
    - [FP Complete Lens tutorial](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
    - [FP Complete tutorial on Lens + JSON](https://www.fpcomplete.com/user/tel/lens-aeson-traversals-prisms)
