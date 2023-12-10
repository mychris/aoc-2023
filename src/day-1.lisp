(defpackage #:aoc-2023/day-1
  (:use #:cl)
  (:export #:trebuchet-1
           #:trebuchet-2)
  (:import-from #:aoc-2023/day-1-input
                #:*input*))

(in-package #:aoc-2023/day-1)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 1: Trebuchet?!")

(defvar *spelled-numbers* '((1 . "one") (2 . "two") (3 . "three")
                            (4 . "four") (5 . "five") (6 . "six")
                            (7 . "seven") (8 . "eight") (9 . "nine")))

(defun spelled-at-p (line idx)
  (loop :for spelled-number :in *spelled-numbers*
        :if (and (char= (aref (cdr spelled-number) 0) (aref line idx))
                 (string= line (cdr spelled-number)
                          :start1 idx
                          :end1 (min (length line) (+ idx (length (cdr spelled-number))))))
          :return (car spelled-number)))

(defun digit-at-p (line idx)
  (digit-char-p (aref line idx)))

(defun find-first-last (predicate line)
  (cons (loop :for index :from 0 :below (length line)
              :for result = (funcall predicate line index)
              :if result :return result)
        (loop :for index :downfrom (1- (length line)) :to 0
              :for result = (funcall predicate line index)
              :if result :return result)))

(defun accum-digits (c)
  (+ (* 10 (car c)) (cdr c)))

(defun parse-from-lines (predicate stream)
  (loop :while (peek-char nil stream nil)
        :collect (find-first-last predicate (read-line stream))))

(defun trebuchet-1 (&optional (stream (make-string-input-stream *input*)))
  "55123"
  (reduce #'+ (map 'list #'accum-digits (parse-from-lines #'digit-at-p stream))))

(defun trebuchet-2 (&optional (stream (make-string-input-stream *input*)))
  "55260"
  (reduce #'+ (map 'list #'accum-digits
                   (parse-from-lines #'(lambda (line idx)
                                         (or (digit-at-p line idx)
                                             (spelled-at-p line idx)))
                                     stream))))
