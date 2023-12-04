(in-package #:cl-user)

(defmacro defpackage-aoc (day name doc)
  `(progn
     (defpackage ,(intern (string-upcase (format nil "aoc-2023.~A" day)) :keyword)
       (:use #:cl)
       (:export ,(intern (string-upcase (format nil "~A-1" name)) :keyword)
                ,(intern (string-upcase (format nil "~A-2" name)) :keyword)))
     (setf
      (documentation (find-package ,(intern (string-upcase (format nil "aoc-2023.~A" day)) :keyword)) t)
      ,doc)))

(defpackage #:aoc-2023
  (:use #:cl))
(setf (documentation (find-package :aoc-2023) t) "Advent of Code 2023")

(defpackage-aoc d1 trebuchet "Day 1: Trebuchet?!")
(defpackage-aoc d2 cube-conundrum "Day 2: Cube Conundrum")
(defpackage-aoc d3 gear-ratios "Day 3: Gear Ratios")
(defpackage-aoc d4 scratchcards "Day 4: Scratchcards")

(in-package #:aoc-2023)

(loop :for day :upfrom 1
      :while (find-package (format nil "AOC-2023.D~A" day))
      :do (do-external-symbols (s (find-package (format nil "AOC-2023.D~A" day)))
            (import s)
            (export (find-symbol (string s) (find-package "AOC-2023")))))
