(defpackage #:aoc-2023/examples/day-8
  (:use #:cl)
  (:export #:*day-8-input*
           #:*day-8-expected*))

(in-package #:aoc-2023/examples/day-8)

(defvar *day-8-expected* (cons 2 6))

(defvar *day-8-input*
  "LR

AAA = (11B, XXX)
11B = (XXX, ZZZ)
ZZZ = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")
