(defpackage #:aoc-2023/day-7
  (:use #:cl)
  (:export #:camel-cards-1
           #:camel-cards-2)
  (:import-from #:aoc-2023/day-7-input
                #:*input*))

(in-package #:aoc-2023/day-7)

(setf (documentation *package* t) "Day 7: Camel Cards")

(defun card-value (card)
  (case card
    (#\A 14)
    (#\K 13)
    (#\Q 12)
    (#\J 11)
    (#\T 10)
    (#\* 0)
    (t (- (char-code card) 48))))

(defun hand-string-to-value-list (hand)
  (loop :for card :across hand :collect (card-value card)))

(defun count-same-cards (hand)
  (loop :for card :across hand
        :if (member card result :key #'car)
          :do (loop :for r :in result :if (char= (car r) card) :do (incf (cdr r)))
        :else
          :collect (cons card 1) :into result
        :finally (return result)))

(defun hand-type (hand)
  (let ((counted-hand (count-same-cards hand)))
    (cond
      ((= 1 (length counted-hand))
       6)
      ((and (= 2 (length counted-hand))
            (or (= 4 (cdar counted-hand))
                (= 1 (cdar counted-hand))))
       5)
      ((= 2 (length counted-hand))
       4)
      ((and (= 3 (length counted-hand))
            (some (lambda (c) (= 3 (cdr c))) counted-hand))
       3)
      ((= 2 (reduce (lambda (a b) (+ a (if (= 2 (cdr b)) 1 0))) counted-hand :initial-value 0))
       2)
      ((some (lambda (c) (= 2 (cdr c))) counted-hand)
       1)
      (t
       0))))

(defun comp-hands (left right)
  (if (= (hand-type left) (hand-type right))
      (loop :for l :across left
            :for r :across right
            :if (< (card-value l) (card-value r))
              :return t
            :if (> (card-value l) (card-value r))
              :return nil)
      (< (hand-type left) (hand-type right))))

(defun parse-input (stream)
  (loop :for line = (read-line stream nil nil)
        :while line
        :collect (cons (subseq line 0 (position #\Space line))
                       (parse-integer (subseq line (position #\Space line))))))

(defun camel-cards-1 (&optional (stream (make-string-input-stream *input*)))
  "252295678"
  (let ((hands-values (sort (parse-input stream) #'comp-hands :key #'car)))
    (loop :for h :in hands-values
          :for x :from 1
          :sum (* x (cdr h)))))

(defun camel-cards-2 (&optional (stream (make-string-input-stream *input*)))
  "0"
  nil)
