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
   #:trebuchet-1
   #:trebuchet-2))

(in-package #:aoc-2023)
