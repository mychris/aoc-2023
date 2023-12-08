(defpackage #:aoc-2023/day-8
  (:use #:cl)
  (:export #:haunted-wasteland-1
           #:haunted-wasteland-2)
  (:import-from #:aoc-2023/day-8-input
                #:*input*))

(in-package #:aoc-2023/day-8)

(setf (documentation *package* t) "Day 8: Haunted Wasteland")

(defun parse-mapping (stream)
  (loop :with mapping = (make-hash-table :test #'equal)
        :for line = (read-line stream nil nil)
        :for line-stream = (make-string-input-stream
                            (or (substitute-if-not #\Space #'alphanumericp line) ""))
        :while line
        :do (setf (gethash (symbol-name (read line-stream)) mapping)
                  (cons (symbol-name (read line-stream))
                        (symbol-name (read line-stream))))
        :finally (return mapping)))

(defun parse-navigation (stream)
  (prog1 (map 'vector (lambda (chr) (if (char= #\L chr) :left :right)) (read-line stream nil nil))
    (read-line stream nil nil)))

(defun parse (stream)
  (values (parse-navigation stream) (parse-mapping stream)))

(defun count-steps (start end-predicate navigation mapping)
  (loop :for nav-idx :upfrom 0
        :until (funcall end-predicate start)
        :count (setq start (case (aref navigation (mod nav-idx (length navigation)))
                             (:left (car (gethash start mapping)))
                             (:right (cdr (gethash start mapping)))))))

(defun prime-factors (x)
  (loop :for n :upfrom 2
        :while (/= x 1)
        :if (= 0 (mod x n))
          :append (loop :while (= 0 (mod x n))
                        :collect n
                        :do (setq x (/ x n)))))

(defun haunted-wasteland-1 (&optional (stream (make-string-input-stream *input*)))
  "19783"
  (multiple-value-call #'count-steps "AAA" (lambda (cur) (string= cur "ZZZ")) (parse stream)))

(defun haunted-wasteland-2 (&optional (stream (make-string-input-stream *input*)))
  "9177460370549"
  (multiple-value-bind (navigation mapping) (parse stream)
    (let* ((end-pred (lambda (cur) (char= #\Z (char cur (1- (length cur))))))
           (all-steps (loop :for start :being :the :hash-keys :of mapping
                            :if (char= #\A (char start (1- (length start))))
                              :collect (count-steps start end-pred navigation mapping)))
           (all-prime-factors (map 'list #'prime-factors all-steps)))
      ;; Multiply all the steps together
      ;; If all steps share a prime factor, that factor should only be taken once
      ;; Intermediate result might overflow 64-bits
      ;; The loop only works because every prime-factor has a power of 1
      (loop :with result = (reduce #'* all-steps)
            :for prime-factor :in (first all-prime-factors)
            :if (every (lambda (p-list) (member prime-factor p-list)) all-prime-factors)
              :do (setq result (/ result (expt prime-factor (1- (length all-steps)))))
            :finally (return result)))))
