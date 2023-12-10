(defpackage #:aoc-2023/day-8
  (:use #:cl)
  (:export #:haunted-wasteland-1
           #:haunted-wasteland-2)
  (:import-from #:aoc-2023/day-8-input
                #:*input*))

(in-package #:aoc-2023/day-8)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 8: Haunted Wasteland")

(defun read-node (line start)
  (subseq line start (+ start 3)))

(defun parse-mapping (stream)
  (let ((mapping (loop :with mapping = (make-hash-table :test #'equal)
                       :for line = (read-line stream nil nil)
                       :while line
                       :do (let ((node-name (read-node line 0)))
                             (setf (gethash node-name mapping)
                                   (list node-name (read-node line 7) (read-node line 12))))
                       :finally (return mapping))))
    (loop :for value :being :the :hash-values :of mapping
          :do (setf (nth 1 value) (gethash (nth 1 value) mapping))
              (setf (nth 2 value) (gethash (nth 2 value) mapping))
          :finally (return mapping))))

(defun parse-navigation (stream)
  (prog1 (read-line stream nil nil)
    (read-line stream nil nil)))

(defun parse (stream)
  (values (parse-navigation stream) (parse-mapping stream)))

(defun count-steps (start end-predicate navigation mapping)
  (setq start (gethash start mapping))
  (loop :for nav-idx :upfrom 0
        :until (funcall end-predicate (nth 0 start))
        :count (setq start (case (aref navigation (mod nav-idx (length navigation)))
                             (#\L (nth 1 start))
                             (#\R (nth 2 start))))))

(defun prime-factors (x)
  (loop :for n :upfrom 3 :by 2
        :while (/= x 1)
        :if (= 0 (mod x n))
          :append (loop :while (= 0 (mod x n))
                        :collect n
                        :do (setq x (/ x n)))))

(defun haunted-wasteland-1 (&optional (stream (make-string-input-stream *input*)))
  "19783"
  (multiple-value-call #'count-steps "AAA" (lambda (cur) (and (char= #\Z (char cur 0))
                                                              (string= cur "ZZZ")))
    (parse stream)))

(defun haunted-wasteland-2 (&optional (stream (make-string-input-stream *input*)))
  "9177460370549"
  (multiple-value-bind (navigation mapping) (parse stream)
    (let* ((end-pred (lambda (cur) (char= #\Z (char cur 2))))
           (all-steps (loop :for start :being :the :hash-keys :of mapping
                            :if (char= #\A (char start 2))
                              :collect (count-steps start end-pred navigation mapping)))
           (all-prime-factors (map 'list #'prime-factors all-steps)))
      ;; Multiply all the steps together
      ;; If all steps share a prime factor, that factor should only be taken once
      ;; Intermediate result might overflow 64-bits
      ;; The loop only works because every prime-factor has a power of 1
      (loop :with result = (reduce #'* all-steps)
            :for prime-factor :in (first all-prime-factors)
            :if (every (lambda (p-list) (find prime-factor p-list)) all-prime-factors)
              :do (setq result (/ result (expt prime-factor (1- (length all-steps)))))
            :finally (return result)))))
