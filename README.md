# JSON command-line utility in Haskell

Jp is command-line utility to parse and transform JSON. JSON inputs
can be filtered and transformed with Haskell Lens expressions.

## Installation

- Copy ```config/jp/modules``` into ```$HOME/.config/jp/modules```
- Execute this command in the shell to make cabal sandboxing happy:

        source ./scripts/sandbox.sh
  
## Example of usages

    cat ./tests/simple.json | sandbox exec ./dist/build/jp/jp -p 
    sandbox exec ./dist/build/jp/jp ./tests/obj1.json -e '<&> members . _Number *~ 10'


## Pretty printing

Resulting JSON expressions are pretty-printed with colors.

## Lens expression

[Lens](https://github.com/ekmett/lens) expressions can be used to filter and modify the input.

Lens tutorials:
- [FP Complete Lens tutorial](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
- [FP Complete tutorial on Lens + JSON](https://www.fpcomplete.com/user/tel/lens-aeson-traversals-prisms)

## Screenshots

### Applying a Lens expression

![Lens expression](https://raw.githubusercontent.com/kototama/jp/master/screenshots/lens.png)
   
### Simple Pretty Printing

![Simple pretty printing](https://raw.githubusercontent.com/kototama/jp/master/screenshots/simple.png)
