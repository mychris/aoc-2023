(defpackage #:aoc-2023/day-9
  (:use #:cl)
  (:export #:mirage-maintenance-1
           #:mirage-maintenance-2)
  (:import-from #:aoc-2023/day-9-input
                #:*input*))

(in-package #:aoc-2023/day-9)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 9: Mirage Maintenance")

(defun parse-line (string &optional (start 0))
  (loop :while (< start (length string))
        :for parsed = (multiple-value-list (parse-integer string :start start :junk-allowed t))
        :if (nth 0 parsed)
          :collect (nth 0 parsed)
        :do (setq start (nth 1 parsed))))

(defun extrapolate (numbers)
  (if (every (lambda (x) (= x (car numbers))) numbers)
      (cons (car numbers) (car numbers))
      (let ((extrapolation (extrapolate (loop :for (left right) :on numbers
                                              :while right
                                              :collect (- right left)))))
        (cons (- (car numbers) (car extrapolation))
              (+ (cdr extrapolation) (car (last numbers)))))))

(defun mirage-maintenance-1 (&optional (stream (make-string-input-stream *input*)))
  "1921197370"
  (loop :for line = (read-line stream nil nil)
        :while line
        :sum (cdr (extrapolate (parse-line line)))))

(defun mirage-maintenance-2 (&optional (stream (make-string-input-stream *input*)))
  "1124"
  (loop :for line = (read-line stream nil nil)
        :while line
        :sum (car (extrapolate (parse-line line)))))
