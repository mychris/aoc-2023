(defpackage #:aoc-2023/examples/day-20
  (:use #:cl)
  (:export #:*day-20-input*
           #:*day-20-expected*))

(in-package #:aoc-2023/examples/day-20)

(defvar *day-20-expected* (cons 32000000 0))

(defvar *day-20-input*
  "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")
