# Advent of Code 2023

Utilities for Advent of Code solutions written in [Haskell](https://www.haskell.org/) especially with [Haskell Language Server](https://github.com/haskell/haskell-language-server) and its [Eval Plugin](https://hackage.haskell.org/package/hls-eval-plugin).

## Usage

### Every advent day

1. Log in to [Advent of Code](https://adventofcode.com/)
1. Copy session HTTP cookie value to `.session` file in this project directory
1. Code
1. In the code insert HLS-powered comment `-- >>> answer [year] [day] [puzzle (1 or 2)] [solution]` that will do the following: download and safe locally input file for given puzzle, run solution against the input using provided *solution* function (present in the current module scope, of type `Show a => String -> a`), print result as in-line comment and write it to system clipboard as text.