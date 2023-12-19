(defpackage #:aoc-2023
  (:use #:cl
        #:aoc-2023-data
        #:aoc-2023/src/day-1
        #:aoc-2023/src/day-2
        #:aoc-2023/src/day-3
        #:aoc-2023/src/day-4
        #:aoc-2023/src/day-5
        #:aoc-2023/src/day-6
        #:aoc-2023/src/day-7
        #:aoc-2023/src/day-8
        #:aoc-2023/src/day-9
        #:aoc-2023/src/day-10
        #:aoc-2023/src/day-11
        #:aoc-2023/src/day-12
        #:aoc-2023/src/day-13
        #:aoc-2023/src/day-14
        #:aoc-2023/src/day-15
        #:aoc-2023/src/day-16
        #:aoc-2023/src/day-17
        #:aoc-2023/src/day-18
        #:aoc-2023/src/day-19))

(in-package #:aoc-2023)

(setf (documentation *package* t) "Advent of Code 2023")

(loop :for day :upfrom 1
      :for day-package = (find-package (format nil "AOC-2023/SRC/DAY-~A" day))
      :while day-package
      :do
         (loop :for s :being :the :external-symbols :in day-package
               :do (export (find-symbol (string s) *package*))))
