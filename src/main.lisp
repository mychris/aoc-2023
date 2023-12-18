(defpackage #:aoc-2023
  (:use #:cl
        #:aoc-2023/day-1
        #:aoc-2023/day-2
        #:aoc-2023/day-3
        #:aoc-2023/day-4
        #:aoc-2023/day-5
        #:aoc-2023/day-6
        #:aoc-2023/day-7
        #:aoc-2023/day-8
        #:aoc-2023/day-9
        #:aoc-2023/day-10
        #:aoc-2023/day-11
        #:aoc-2023/day-12
        #:aoc-2023/day-13
        #:aoc-2023/day-14
        #:aoc-2023/day-15
        #:aoc-2023/day-16
        #:aoc-2023/day-17
        #:aoc-2023/day-18))

(in-package #:aoc-2023)

(setf (documentation *package* t) "Advent of Code 2023")

(loop :for day :upfrom 1
      :for day-package = (find-package (format nil "AOC-2023/DAY-~A" day))
      :while day-package
      :do
         (loop :for s :being :the :external-symbols :in day-package
               :do (export (find-symbol (string s) *package*))))
