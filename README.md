# sudoku-solver
A Sudoku solver in Haskell

Algorithm came from [this blog post](https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/).
Implemented by my own code as much as possible.

Source file of Sudoku puzzles is [here](https://abhinavsarkar.net/files/sudoku17.txt.bz2)

# Usage

```shell-session
$ stack build
$ head sudoku17.txt | .stack-work/dist/x86_64-osx/Cabal-2.2.0.1/build/sudoku-solver-exe/sudoku-solver-exe
$ head -n 100 sudoku17.txt | time .stack-work/dist/x86_64-osx/Cabal-2.2.0.1/build/sudoku-solver-exe/sudoku-solver-exe
```
