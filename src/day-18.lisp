(defpackage #:aoc-2023/day-18
  (:use #:cl)
  (:export #:lavaduct-lagoon-1
           #:lavaduct-lagoon-2))

(in-package #:aoc-2023/day-18)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 18: Lavaduct Lagoon")

(defun parse (stream)
  (loop :for line = (read-line stream nil nil)
        :while line
        :collect (list (char line 0)
                       (parse-integer line :start 2 :junk-allowed t)
                       (subseq line (1+ (position #\# line)) (1- (length line))))))

(defun fix-plan (plan)
  (loop :for item :in plan
        :for color = (nth 2 item)
        :collect (list (case (char color 5)
                         (#\0 #\R)
                         (#\1 #\D)
                         (#\2 #\L)
                         (#\3 #\U))
                       (parse-integer color :start 0 :end 5 :radix 16))))

(defun plan-to-points (plan)
  (loop :with x = 0
        :and y = 0
        :for (direction steps) :in plan
        :collect (case direction
                   (#\R (cons (incf x steps) y))
                   (#\L (cons (decf x steps) y))
                   (#\D (cons x (incf y steps)))
                   (#\U (cons x (decf y steps))))))

(defun get-area (plan)
  (let ((perimeter (reduce #'+ plan :key #'cadr))
        (area (abs (/ (loop :for ((x1 . y1) (x2 . y2)) :on (plan-to-points plan)
                            :while x2
                            :sum (* (- x1 x2) (+ y1 y2)))
                      2))))
    (+ 1 area (/ perimeter 2))))

(defun lavaduct-lagoon-1 (stream)
  (get-area (parse stream)))

(defun lavaduct-lagoon-2 (stream)
  (get-area (fix-plan (parse stream))))
