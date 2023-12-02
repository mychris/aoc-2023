# Advent of Code 2023

Lisp system with AoC 2023 solutions.

Developed `sbcl` but should work with every Common Lisp implementation:

* sbcl `sbcl --non-interactive --eval '(load "run.lisp")'`
* clisp `clisp -x '(load "run.lisp")'`
* ccl `ccl --eval '(load "run.lisp")' --eval '(quit)'`
* cmucl `cmucl -eval '(load "run.lisp")' -eval '(quit)'`
* ecl `ecl --eval '(load "run.lisp")' --eval '(quit)'`
* clasp `clasp --quti --eval '(load "run.lisp")'`
* abcl `abcl --batch --eval '(load "run.lisp")'`

```
* (ql:quickload "aoc-2023")
* (aoc-2023:trebuchet-1)
* (describe (find-package "AOC-2023") t)
```
