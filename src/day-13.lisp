(defpackage #:aoc-2023/src/day-13
  (:use #:cl)
  (:export #:point-of-incidence-1
           #:point-of-incidence-2)
  (:import-from #:aoc-2023-data
                #:*day-13-input*))

(in-package #:aoc-2023/src/day-13)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 13: Point of Incidence")

(defun parse (stream)
  (loop :with result = (make-array 0 :fill-pointer 0 :adjustable t)
        :for line = (read-line stream nil nil)
        :while (and line (/= 0 (length line)))
        :do (vector-push-extend line result)
        :finally (return result)))

(defun horizontal-reflect-p (matrix row width height)
  (loop :for up :downfrom row :to 0
        :for down :upfrom (1+ row) :below height
        :sum (loop :for col :upfrom 0 :below width
                   :count (char/= (aref (aref matrix up) col)
                                  (aref (aref matrix down) col)))))

(defun vertical-reflect-p (matrix col width height)
  (loop :for left :downfrom col :to 0
        :for right :upfrom (1+ col) :below width
        :sum (loop :for row :upfrom 0 :below height
                   :count (char/= (aref (aref matrix row) left)
                                  (aref (aref matrix row) right)))))

(defun find-reflection (matrix smudges)
  (let ((width (length (aref matrix 0)))
        (height (length matrix)))
    (loop :for x :upfrom 0 :below (max (1- width) (1- height))
          :when (and (< x (1- width))
                     (= smudges (vertical-reflect-p matrix x width height)))
            :return (1+ x)
          :when (and (< x (1- height))
                     (= smudges (horizontal-reflect-p matrix x width height)))
            :return (* 100 (1+ x))
          :finally (error "Nothing found"))))

(defun point-of-incidence-1 (&optional (stream (make-string-input-stream *day-13-input*)))
  (loop :for matrix = (parse stream)
        :while (< 0 (length matrix))
        :sum (find-reflection matrix 0)))

(defun point-of-incidence-2 (&optional (stream (make-string-input-stream *day-13-input*)))
  (loop :for matrix = (parse stream)
        :while (< 0 (length matrix))
        :sum (find-reflection matrix 1)))
