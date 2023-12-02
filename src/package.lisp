(in-package #:cl-user)

(defpackage #:aoc-2023-d1
  (:use #:cl)
  (:export
   #:trebuchet-1
   #:trebuchet-2))

(defpackage #:aoc-2023
  (:use #:cl)
  (:import-from #:aoc-2023-d1
                #:trebuchet-1
                #:trebuchet-2)
  (:export
   #:day1-1
   #:trebuchet-1
   #:day1-2
   #:trebuchet-2))

(in-package #:aoc-2023)

(defun day1-1 () (trebuchet-1))
(defun day1-2 () (trebuchet-2))
