(defpackage #:aoc-2023/day-7
  (:use #:cl)
  (:export #:camel-cards-1
           #:camel-cards-2)
  (:import-from #:aoc-2023/day-7-input
                #:*input*))

(in-package #:aoc-2023/day-7)

(setf (documentation *package* t) "Day 7: Camel Cards")

(defun parse-input (stream &optional (map-hand #'identity))
  (loop :for line = (read-line stream nil nil)
        :while line
        :collect (cons (funcall map-hand (subseq line 0 (position #\Space line)))
                       (parse-integer (subseq line (position #\Space line))))))

(defun card-value (card)
  (case card
    (#\A 14)
    (#\K 13)
    (#\Q 12)
    (#\J 11)
    (#\T 10)
    (#\* 0)
    (t (- (char-code card) 48))))

(defun count-same-cards (hand)
  (sort (loop :for card :across hand
              :if (member card result :key #'car)
                :do (loop :for r :in result :if (char= (car r) card) :do (incf (cdr r)))
              :else
                :collect (cons card 1) :into result
              :finally (return result))
        #'> :key #'cdr))

(defun hand-type (hand)
  (let ((counted-hand (count-same-cards hand)))
    (when (and (member #\* counted-hand :key #'car)
               (> (length counted-hand) 1))
      (if (char= #\* (car (nth 0 counted-hand)))
          (incf (cdadr counted-hand) (cdr (nth 0 counted-hand)))
          (incf (cdar counted-hand) (cdr (find #\* counted-hand :key #'car))))
      (setq counted-hand (remove #\* counted-hand :key #'car)))
    (cond
      ((= 1 (length counted-hand))
       6)
      ((= 4 (cdr (nth 0 counted-hand)))
       5)
      ((= 2 (length counted-hand))
       4)
      ((= 3 (cdr (nth 0 counted-hand)))
       3)
      ((= 2 (cdr (nth 0 counted-hand)))
       (if (= 2 (cdr (nth 1 counted-hand))) 2 1))
      (t
       0))))

(defun comp-hands (left right)
  (let ((left-type (hand-type left))
        (right-type (hand-type right)))
    (if (/= left-type right-type)
        (< left-type right-type)
        (loop :for l :across left
              :for r :across right
              :if (char/= l r)
                :return (< (card-value l) (card-value r))
              :finally (return nil)))))

(defun calc-winnings (hands-values)
  (loop :for h :in (sort hands-values #'comp-hands :key #'car)
        :for x :from 1
        :sum (* x (cdr h))))

(defun camel-cards-1 (&optional (stream (make-string-input-stream *input*)))
  "252295678"
  (calc-winnings (parse-input stream)))

(defun camel-cards-2 (&optional (stream (make-string-input-stream *input*)))
  "250577259"
  (calc-winnings
   (parse-input stream (lambda (hand)
                         (map 'string (lambda (chr) (if (char= #\J chr) #\* chr)) hand)))))
