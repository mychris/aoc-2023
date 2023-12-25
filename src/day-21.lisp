(defpackage #:aoc-2023/day-21
  (:use #:cl)
  (:export #:step-counter-1
           #:step-counter-2))

(in-package #:aoc-2023/day-21)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 21: Step Counter")

(defun parse (stream)
  (let ((real (loop :with result = (make-array 0 :fill-pointer 0 :adjustable t)
                    :for line = (read-line stream nil nil)
                    :while line
                    :do (vector-push-extend line result)
                    :finally (return result))))
    (loop :with result = (make-array (list (length real) (length (aref real 0)))
                                     :element-type 'character
                                     :initial-element #\#)
          :and start = nil
          :for line :across real
          :for y :upfrom 0
          :do (loop :for chr :across line
                    :for x :upfrom 0
                    :do (setf (aref result y x) chr)
                    :when (char= chr #\S)
                      :do (setq start (cons x y))
                          (setf (aref result y x) #\.))
          :finally (return (values result start)))))

(defun count-steps (map start max-steps)
  (let ((visited (make-array (list (+ (cdr start) max-steps max-steps 4)
                                   (+ (car start) max-steps max-steps 4))
                             :initial-element -1))
        (result 0)
        (even-odd (mod max-steps 2)))
    (loop :with stack = (list (list 0 start))
          :for head = (pop stack)
          :for step-count = (nth 0 head)
          :for x = (car (nth 1 head))
          :for y = (cdr (nth 1 head))
          :for this-old-count = (aref visited (+ max-steps (or y 0)) (+ max-steps (or x 0)))
          :while step-count
          :do (when (and (char= (aref map (mod y (array-dimension map 0)) (mod x (array-dimension map 1))) #\.)
                         (or (= -1 this-old-count)
                             (< step-count this-old-count)))
                (when (and (= -1 this-old-count)
                           (= even-odd (mod step-count 2)))
                  (incf result))
                (setf (aref visited (+ max-steps y) (+ max-steps x)) step-count)
                (when (<= step-count max-steps)
                  (incf step-count)
                  (push (list step-count (cons (1+ x) y)) stack)
                  (push (list step-count (cons x (1+ y))) stack)
                  (push (list step-count (cons (1- x) y)) stack)
                  (push (list step-count (cons x (1- y))) stack)))
          :finally (return result))))

(defun q (y0 y1 y2 n)
  (let* ((a (floor (+ (- y2 (* 2 y1)) y0) 2))
         (b (- y1 y0 a))
         (c y0))
    (+ (* a (* n n)) (* b n) c)))

(defun step-counter-1 (stream)
  (multiple-value-call #'count-steps (parse stream) 64))

(defun step-counter-2 (stream)
  (multiple-value-bind (map start)
      (parse stream)
    (let* ((steps 26501365)
           (width (array-dimension map 1))
           (ys (loop :for n :upfrom 0 :below 3
                     :for steps = (+ (floor width 2) (* n width))
                     :collect (count-steps map start steps))))
      (q (nth 0 ys) (nth 1 ys) (nth 2 ys)
         (floor (- steps (floor width 2)) width)))))
