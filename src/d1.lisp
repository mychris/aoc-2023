(in-package #:aoc-2023.d1)

(defvar *spelled-numbers* (list (cons 1 "one")
                                (cons 2 "two")
                                (cons 3 "three")
                                (cons 4 "four")
                                (cons 5 "five")
                                (cons 6 "six")
                                (cons 7 "seven")
                                (cons 8 "eight")
                                (cons 9 "nine")))

(defun trebuchet-spelled (line idx)
  (car
   (find-if #'(lambda (spelled-number)
                (and (>= idx (1- (length (cdr spelled-number))))
                     (string= line (cdr spelled-number)
                              :start1 (- idx (1- (length (cdr spelled-number))))
                              :end1 (1+ idx))))
            *spelled-numbers*)))

(defun trebuchet-digit (line idx)
  (digit-char-p (aref line idx)))

(defun find-first-last (predicate line)
  (let ((result-list (loop :for x :from 0 :below (length line)
                           :for result = (funcall predicate line x)
                           :if result
                             :collect result)))
    (cons (first result-list) (car (last result-list)))))

(defun trebuchet-accum-digits (c)
  (+ (* 10 (car c)) (cdr c)))

(defun trebuchet-get-from-lines (predicate accum stream)
  (loop :for line = (read-line stream nil nil)
        :while line
        :collect (funcall accum (find-first-last predicate line))))

(defun trebuchet-1 (&optional stream)
  (reduce #'+ (trebuchet-get-from-lines
               #'trebuchet-digit
               #'trebuchet-accum-digits
               (or stream (make-string-input-stream *d1-input*)))))

(defun trebuchet-2 (&optional stream)
  (reduce #'+ (trebuchet-get-from-lines
               #'(lambda (line idx)
                   (or
                    (trebuchet-digit line idx)
                    (trebuchet-spelled line idx)))
               #'trebuchet-accum-digits
               (or stream (make-string-input-stream *d1-input*)))))
