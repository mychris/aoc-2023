(defpackage #:aoc-2023/day-22
  (:use #:cl)
  (:export #:sand-slabs-1
           #:sand-slabs-2))

(in-package #:aoc-2023/day-22)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 22: Sand Slabs")

(defstruct point (x) (y) (z))
(defstruct brick (p1) (p2))

(defun parse-point (line start end)
  (let* ((c1 (position #\, line :start start))
         (c2 (position #\, line :start (1+ c1))))
    (make-point :x (parse-integer line :start start :end c1)
                :y (parse-integer line :start (1+ c1) :end c2)
                :z (parse-integer line :start (1+ c2) :end end))))

(defun parse (stream)
  (sort (loop :for line = (read-line stream nil nil)
              :while line
              :collect (let ((p1 (parse-point line 0 (position #\~ line)))
                             (p2 (parse-point line (1+ (position #\~ line)) (length line))))
                         (make-brick :p1 (make-point :x (min (point-x p1) (point-x p2))
                                                     :y (min (point-y p1) (point-y p2))
                                                     :z (min (point-z p1) (point-z p2)))
                                     :p2 (make-point :x (max (point-x p1) (point-x p2))
                                                     :y (max (point-y p1) (point-y p2))
                                                     :z (max (point-z p1) (point-z p2))))))
        (lambda (left right)
          (< (point-z (brick-p1 left)) (point-z (brick-p1 right))))))

(defun fall-single (brick hight-map)
  (let* ((peak (loop :for x :upfrom (point-x (brick-p1 brick)) :to (point-x (brick-p2 brick))
                     :maximize
                     (loop :for y :upfrom (point-y (brick-p1 brick)) :to (point-y (brick-p2 brick))
                           :maximize (aref hight-map y x))))
         (diff (max (- (point-z (brick-p1 brick)) peak 1) 0)))
    (make-brick :p1 (make-point :x (point-x (brick-p1 brick))
                                :y (point-y (brick-p1 brick))
                                :z (- (point-z (brick-p1 brick)) diff))
                :p2 (make-point :x (point-x (brick-p2 brick))
                                :y (point-y (brick-p2 brick))
                                :z (- (point-z (brick-p2 brick)) diff)))))

(defun fall (bricks)
  (loop :with hight-map = (make-array '(10 10) :initial-element 0)
        :for brick :in bricks
        :for fallen-brick = (fall-single brick hight-map)
        :count (/= (point-z (brick-p1 brick)) (point-z (brick-p1 fallen-brick)))
          :into fall-count
        :do (loop :for x :upfrom (point-x (brick-p1 brick)) :to (point-x (brick-p2 brick))
                  :do (loop :for y :upfrom (point-y (brick-p1 brick)) :to (point-y (brick-p2 brick))
                            :do (setf (aref hight-map y x) (point-z (brick-p2 fallen-brick)))))
        :collect fallen-brick
          :into result
        :finally (return (values fall-count result))))

(defun sand-slabs (stream)
  (let ((bricks (nth-value 1 (fall (parse stream)))))
    (loop :for brick :in bricks
          :for falling = (nth-value 0 (fall (remove brick bricks)))
          :count (= 0 falling)
            :into one
          :sum falling
            :into two
          :finally (return (values one two)))))

(defun sand-slabs-1 (stream) (nth-value 0 (sand-slabs stream)))

(defun sand-slabs-2 (stream) (nth-value 1 (sand-slabs stream)))
