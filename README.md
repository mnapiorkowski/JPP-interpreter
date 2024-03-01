# JPP - Risotto language interpreter
Programming Languages and Paradigms - 6. semester @ MIMUW

## Language description
Risotto is the imperative language with static type checking. It supports three data types: `int`, `bool` and `string`. Program in Risotto is a list of functions. Arguments can be passed by value or by reference. Function can either end with `return` or `turnback`. The latter is a special instruction that causes the entire function to be executed again but in reverse order of instructions. Datailed description of the language can be found in `Risotto.pdf`. Examples of programs written in Risotto are in the `test` directory.

## Prerequisites
BNFC (tested on version 8.8.4) and GHC (tested on version 2.9.4.1)

## How to run
```
make
./interpreter foo/bar/baz.ris
```
