(defpackage #:aoc-2023-data
  (:use #:cl
        #:aoc-2023/examples/day-1
        #:aoc-2023/examples/day-2
        #:aoc-2023/examples/day-3
        #:aoc-2023/examples/day-4
        #:aoc-2023/examples/day-5
        #:aoc-2023/examples/day-6
        #:aoc-2023/examples/day-7
        #:aoc-2023/examples/day-8
        #:aoc-2023/examples/day-9
        #:aoc-2023/examples/day-10
        #:aoc-2023/examples/day-11
        #:aoc-2023/examples/day-12
        #:aoc-2023/examples/day-13
        #:aoc-2023/examples/day-14
        #:aoc-2023/examples/day-15
        #:aoc-2023/examples/day-16
        #:aoc-2023/examples/day-17
        #:aoc-2023/examples/day-18
        #:aoc-2023/examples/day-19
        #:aoc-2023/examples/day-20))

(in-package #:aoc-2023-data)

(setf (documentation *package* t) "Advent of Code 2023 data")

(loop :for day :upfrom 1
      :for day-package = (find-package (format nil "AOC-2023/EXAMPLES/DAY-~A" day))
      :while day-package
      :do
         (loop :for s :being :the :external-symbols :in day-package
               :do (export (find-symbol (string s) *package*))))
