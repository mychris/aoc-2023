# Advent of Code 2023

Common Lisp system with AoC 2023 solutions.

Developed using `sbcl` but should work with every Common Lisp implementation.

```
* (ql:quickload "aoc-2023")
* (aoc-2023:trebuchet-1)
* (describe (find-package :aoc-2023) t)
```

Use `run.lisp` to run everything:

* sbcl `sbcl --noinform --non-interactive --eval '(load "run.lisp")'`
* clisp `clisp -q -q -x '(progn (load "run.lisp" :verbose nil) (quit))'`
* clozure cl `ccl --eval '(progn (load "run.lisp") (quit))'`
* cmucl `cmucl -quiet -eval '(progn (load "run.lisp") (quit))'`
* ecl `ecl -q --eval '(progn (load "run.lisp") (quit))'`
* clasp `clasp --noinform --quit --eval '(load "run.lisp")'`
* abcl `abcl --noinform --nosystem --batch --eval '(load "run.lisp")'`
* allegro cl `alisp -e '(setq *load-verbose* nil)' -L ~/.clinit.cl -L run.lisp --kill`

# Links

* https://adventofcode.com/2023

# License

Copyright (c) 2023 Christoph Göttschkes

Licensed under the [MIT](https://opensource.org/licenses/MIT) License.
See the `LICENSE` file for more info.
