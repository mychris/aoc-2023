(in-package #:cl-user)

(defpackage #:aoc-2023-d1
  (:use #:cl)
  (:export
   #:trebuchet-1
   #:trebuchet-2))

(defpackage #:aoc-2023-d2
  (:use #:cl)
  (:export
   #:cube-conundrum-1
   #:cube-conundrum-2))

(defpackage #:aoc-2023-d3
  (:use #:cl)
  (:export
   #:gear-ratios-1
   #:gear-ratios-2))

(defpackage #:aoc-2023
  (:use #:cl)
  (:import-from #:aoc-2023-d1
                #:trebuchet-1
                #:trebuchet-2)
  (:import-from #:aoc-2023-d2
                #:cube-conundrum-1
                #:cube-conundrum-2)
  (:import-from #:aoc-2023-d3
                #:gear-ratios-1
                #:gear-ratios-2)
  (:export
   #:trebuchet-1
   #:trebuchet-2
   #:cube-conundrum-1
   #:cube-conundrum-2
   #:gear-ratios-1
   #:gear-ratios-2))

(in-package #:aoc-2023)
