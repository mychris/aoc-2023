(defpackage #:aoc-2023/day-21
  (:use #:cl)
  (:export #:step-counter-1
           #:step-counter-2))

(in-package #:aoc-2023/day-21)

(declaim (optimize (speed 0) (debug 3) (safety 3)))
(setf (documentation *package* t) "Day 21: Step Counter")

(defmacro make-pos (x y) `(cons ,x ,y))

(defun parse (stream)
  (let ((real (loop :with result = (make-array 0 :fill-pointer 0 :adjustable t)
                    :for line = (read-line stream nil nil)
                    :while line
                    :do (vector-push-extend line result)
                    :finally (return result))))
    (loop :with result = (make-array (list (+ 2 (length real)) (+ 2 (length (aref real 0))))
                                     :element-type 'character
                                     :initial-element #\#)
          :and start = nil
          :for line :across real
          :for y :upfrom 1
          :do (loop :for chr :across line
                    :for x :upfrom 1
                    :do (setf (aref result y x) chr)
                    :when (char= chr #\S)
                      :do (setq start (make-pos x y)))
          :finally (return (values result start)))))

(defun expand (map factor)
  (loop :with result = (make-array (list (* factor (array-dimension map 0))
                                         (* factor (array-dimension map 1))))
        :for y :upfrom 0 :below (array-dimension result 0)
        :do (loop :for x :upfrom 0 :below (array-dimension result 1)
                  :do (setf (aref result y x) (aref map
                                                    (mod y (array-dimension map 0))
                                                    (mod x (array-dimension map 1)))))))

(defun flood-fill-even-odd (map start)
  (setf (aref map (cdr start) (car start)) #\.)
  (loop :with queue = (list (list #\E start))
        :for (even-odd (x . y)) = (pop queue)
        :while even-odd
        :when (char= (aref map y x) #\.)
          :do (setf (aref map y x) even-odd)
              (setq even-odd (if (char= even-odd #\E) #\O #\E))
              (push (list even-odd (make-pos (1+ x) y)) queue)
              (push (list even-odd (make-pos x (1+ y))) queue)
              (push (list even-odd (make-pos (1- x) y)) queue)
              (push (list even-odd (make-pos x (1- y))) queue)))

(defun count-steps (filled-map start max-steps)
  (let ((target-char (if (= 0 (mod max-steps 2)) #\E #\O)))
    (loop :with queue = (list (list 0 start))
          :for (step-count (x . y)) = (pop queue)
          :while (and step-count (<= step-count max-steps))
          :count (char= (aref filled-map y x) target-char)
          :when (or (char= (aref filled-map y x) #\E)
                    (char= (aref filled-map y x) #\O))
            :do (incf step-count)
                (setf (aref filled-map y x) (char-downcase (aref filled-map y x)))
                (setq queue (append queue (list (list step-count (make-pos (1+ x) y))
                                                (list step-count (make-pos x (1+ y)))
                                                (list step-count (make-pos (1- x) y))
                                                (list step-count (make-pos x (1- y)))))))))

(defun step-counter-1 (stream)
  (multiple-value-bind (map start)
      (parse stream)
    (flood-fill-even-odd map start)
    (count-steps map start 64)))
