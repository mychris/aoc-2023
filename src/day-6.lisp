(defpackage #:aoc-2023/day-6
  (:use #:cl)
  (:export #:wait-for-it-1
           #:wait-for-it-2)
  (:import-from #:aoc-2023/day-6-input
                #:*input*))

(in-package #:aoc-2023/day-6)

(setf (documentation *package* t) "Day 6: Wait For It")

(defun parse-ints (str)
  (loop :for idx :from 0 :below (length str)
        :append (multiple-value-bind (number end)
                    (parse-integer str :start idx :junk-allowed t)
                  (setq idx end)
                  (if number (list number) number))))

(defun concatenate-numbers (number-seq)
  (reduce (lambda (x y) (+ (* x (expt 10 (1+ (truncate (log y 10))))) y)) number-seq))

(defun winning-strategies (time distance)
  "Return cons cell with (minimum . maximum) time to hold for a win.
Binary search for the minimum time.  Then calculate the maximum from it."
  (loop :with low = 1
        :with high = (truncate (/ time 2))
        :while (< 1 (- high low))
        :for this-hold = (truncate (/ (+ high low) 2))
        :for this-distance = (* this-hold (- time this-hold))
        :if (> this-distance distance)
          :do (setq high this-hold)
        :else
          :do (setq low this-hold)
        :finally (return (cons high (1+ (- time high))))))

(defun all-winning-strategies (times distances)
  (map 'list #'winning-strategies times distances))

(defun count-winning-strategies (strategies)
  (map 'list (lambda (x) (- (cdr x) (car x))) strategies))

(defun solve (times distances)
  (reduce #'* (count-winning-strategies (all-winning-strategies times distances))))

(defun wait-for-it-1 (&optional (stream (make-string-input-stream *input*)))
  "840336"
  (solve (parse-ints (read-line stream nil nil))
         (parse-ints (read-line stream nil nil))))

(defun wait-for-it-2 (&optional (stream (make-string-input-stream *input*)))
  "41382569"
  (solve (list (concatenate-numbers (parse-ints (read-line stream nil nil))))
         (list (concatenate-numbers (parse-ints (read-line stream nil nil))))))
