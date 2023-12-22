(defpackage #:aoc-2023/day-11
  (:use #:cl)
  (:export #:cosmic-expansion-1
           #:cosmic-expansion-2))

(in-package #:aoc-2023/day-11)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 11: Cosmic Expansion")

(defun parse-image (stream)
  (loop :with row = 1
        :and col = 1
        :for chr = (read-char stream nil nil)
        :while chr
        :if (char= #\# chr)
          :collect (cons row col)
        :if (char= #\Newline chr)
          :do (setq col 0
                    row (1+ row))
        :do (incf col)))

(defmacro cosmic-expansion-loop (galaxies by key)
  (let ((last (gensym))
        (incr (gensym))
        (galaxy (gensym)))
    `(loop :with ,last = 1
           :and ,incr = 0
           :for ,galaxy = ,galaxies :then (cdr ,galaxy)
           :while ,galaxy
           :if (< ,last (,key ,galaxy))
             :do (incf ,incr (- (* ,by (- (,key ,galaxy) ,last)) (- (,key ,galaxy) ,last)))
           :do (incf (,key ,galaxy) ,incr)
               (setq ,last (+ 1 (,key ,galaxy) (- ,incr))))))

(defun cosmic-expansion (galaxies by)
  (setq galaxies (sort galaxies #'< :key #'cdr))
  (cosmic-expansion-loop galaxies by cdar)
  (setq galaxies (sort galaxies #'< :key #'car))
  (cosmic-expansion-loop galaxies by caar)
  galaxies)

;; really hot code.  Rows are already ordered, so no ABS needed
(declaim (ftype (function (fixnum fixnum fixnum fixnum) fixnum) measure-distance))
(declaim (inline measure-distance))
(defun measure-distance (row1 col1 row2 col2)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (+ (- row2 row1) (abs (- col2 col1))))

(defun sum-galaxies-distances (galaxies)
  (loop :for head = galaxies :then (cdr head)
        :for this-galaxy = (car head)
        :while head
        :sum (loop :for other :in (cdr head)
                   :sum (measure-distance (car this-galaxy) (cdr this-galaxy)
                                          (car other) (cdr other)))))

(defun cosmic-expansion-1 (stream)
  (let ((galaxies (cosmic-expansion (parse-image stream) 2)))
    (sum-galaxies-distances galaxies)))

(defun cosmic-expansion-2 (stream)
  (let ((galaxies (cosmic-expansion (parse-image stream) 1000000)))
    (sum-galaxies-distances galaxies)))
