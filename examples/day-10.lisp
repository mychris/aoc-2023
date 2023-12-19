(defpackage #:aoc-2023/examples/day-10
  (:use #:cl)
  (:export #:*day-10-input*
           #:*day-10-expected*))

(in-package #:aoc-2023/examples/day-10)

(defvar *day-10-expected* (cons 70 8))

(defvar *day-10-input*
  ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...")
