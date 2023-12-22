(defpackage #:aoc-2023/day-2
  (:use #:cl)
  (:export #:cube-conundrum-1
           #:cube-conundrum-2))

(in-package #:aoc-2023/day-2)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 2: Cube Conundrum")

(defun test-round (round configuration)
  (loop :for (cube num) :on round :by #'cddr
        :if (< (getf configuration cube 0) num)
          :do (return nil)
        :finally (return t)))

(defun max-round (round current-max)
  (loop :for (cube num) :on round :by #'cddr
        :if (< (getf current-max cube 0) num)
          :do (setf (getf current-max cube) num)
        :finally (return current-max)))

(defun split (delimiterp str &key (start 0) (end (length str)))
  (loop :for this-beg = (position-if-not delimiterp str :start start :end end)
          :then (position-if-not delimiterp str :start (1+ this-end) :end end)
        :for this-end = (and this-beg (position-if delimiterp str :start this-beg :end end))
        :when this-beg
          :collect (subseq str this-beg this-end)
        :while this-end))

(defun parse-round (str)
  (loop :for elem :in (map 'list
                           #'(lambda (x) (string-trim '(#\Space) x))
                           (split #'(lambda (x) (char= x #\,)) str))
        :collect (intern (string-upcase (subseq elem (1+ (search " " elem :from-end t)))) :keyword)
        :collect (parse-integer elem :end (search " " elem))))

(defun split-out-rounds (str)
  (split #'(lambda (x) (char= x #\;)) str))

(defun split-out-game (str)
  (let ((s (split #'(lambda (x) (char= x #\:)) str)))
    (values (parse-integer (car s) :start (search " " (car s) :from-end t))
            (cadr s))))

(defun apply-sum-games-in (fun stream)
  (loop :while (peek-char nil stream nil)
        :sum (multiple-value-bind (game rounds) (split-out-game (read-line stream))
               (funcall fun game (map 'list #'parse-round (split-out-rounds rounds))))))

(defun cube-conundrum-1 (stream &optional (config '(:red 12 :green 13 :blue 14)))
  (apply-sum-games-in #'(lambda (game rounds)
                          (if (every #'(lambda (round) (test-round round config)) rounds)
                              game
                              0))
                      stream))

(defun cube-conundrum-2 (stream)
  (apply-sum-games-in #'(lambda (game rounds)
                          (declare (ignore game))
                          (apply #'* (remove-if #'keywordp
                                                (reduce #'(lambda (max round) (max-round round max))
                                                        rounds
                                                        :initial-value nil))))
                      stream))
