(defpackage #:aoc-2023/examples/day-12
  (:use #:cl)
  (:export #:*day-12-input*
           #:*day-12-expected*))

(in-package #:aoc-2023/examples/day-12)

(defvar *day-12-expected* (cons 21 525152))

(defvar *day-12-input*
  "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")
