(defpackage #:aoc-2023/day-24
  (:use #:cl)
  (:export #:never-tell-me-the-odds-1))

(in-package #:aoc-2023/day-24)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 24: Never Tell Me The Odds")

(defstruct point (x) (y) (z))
(defstruct hailstone (point) (vel))

(defun parse-line (line)
  (remove-if-not #'identity
                 (loop :with start = 0
                       :while (< start (length line))
                       :collect (multiple-value-bind (num end)
                                    (parse-integer line :start start :junk-allowed t)
                                  (setq start (1+ end))
                                  num))))

(defun parse (stream)
  (loop :for line = (read-line stream nil nil)
        :while line
        :collect (let ((nums (parse-line line)))
                   (make-hailstone
                    :point (make-point :x (nth 0 nums) :y (nth 1 nums) :z (nth 2 nums))
                    :vel (make-point :x (nth 3 nums) :y (nth 4 nums) :z (nth 5 nums))))))

(defun pair (l)
  (loop :for h1 = l :then (cdr h1)
        :while h1
        :append (loop :for h2 = (cdr h1) :then (cdr h2)
                      :while h2
                      :collect (cons (car h1) (car h2)))))

(defun time-at-x (hs x)
  (float (/ (- x (point-x (hailstone-point hs))) (point-x (hailstone-vel hs)))))

(defun calc-m (hs)
  (/ (point-y (hailstone-vel hs)) (point-x (hailstone-vel hs))))

(defun calc-b (hs)
  (- (point-y (hailstone-point hs)) (* (calc-m hs) (point-x (hailstone-point hs)))))

(defun intersect (left right)
  (let ((m-left (calc-m left))
        (m-right (calc-m right)))
    (if (= m-left m-right)
        nil
        (let* ((b-left (calc-b left))
               (b-right (calc-b right))
               (x (/ (- b-left b-right) (- m-right m-left)))
               (y (+ (* m-left x) b-left)))
          (cons (float x) (float y))))))

(defun never-tell-me-the-odds-1 (stream)
  (let* ((stones (parse stream))
         (stone-pairs (pair stones)))
    (loop :for (left . right) :in stone-pairs
          :for (x . y) = (intersect left right)
          :count (and x y
                     (<= 200000000000000 x)
                     (<= x 400000000000000)
                     (<= 200000000000000 y)
                     (<= y 400000000000000)
                     (<= 0 (time-at-x left x))
                     (<= 0 (time-at-x right x))))))
