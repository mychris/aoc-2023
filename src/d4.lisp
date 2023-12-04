(in-package #:aoc-2023.d4)

(defun read-number-list (input-str)
  (loop :with number = 0
        :for chr :across input-str
        :if (and (not (digit-char-p chr))
                 (/= 0 number))
          :collect (prog1 number
                     (setf number 0))
        :if (digit-char-p chr)
          :do (setf number (+ (* 10 number) (digit-char-p chr)))))

(defun read-scratchcards (stream)
  (loop :with result = (make-array 1024 :fill-pointer 0
                                        :adjustable t
                                        :initial-element nil
                                        :element-type 'list)
        :for raw-line = (read-line stream nil nil)
        :while raw-line
        :do (let* ((line (concatenate 'string raw-line " "))
                   (colon (search ":" line))
                   (bar (search "|" line)))
              (vector-push-extend
               (list (read-number-list (subseq line colon bar))
                     (read-number-list (subseq line bar)))
               result))
        :finally (return result)))

(defun number-of-matches (winning-numbers numbers)
  (loop :for num :in numbers :sum (if (member num winning-numbers) 1 0)))

(defun calc-points-of-scratchcard (winning-numbers numbers)
  (ash 1 (1- (number-of-matches winning-numbers numbers))))

(defun scratchcards-1 (&optional stream)
  (loop :for card :across (read-scratchcards (or stream (make-string-input-stream *d4-input*)))
        :sum (calc-points-of-scratchcard (nth 0 card) (nth 1 card))))

(defun scratchcards-2 (&optional stream)
  (loop :with all-cards = (read-scratchcards (or stream (make-string-input-stream *d4-input*)))
        :with each-card-times = (make-array (length all-cards) :initial-element 1)
        :for idx :upfrom 0 :below (length all-cards)
        :for this-card = (aref all-cards idx)
        :for n-matches = (number-of-matches (nth 0 this-card) (nth 1 this-card))
        :do (loop :for idx-2 :upfrom (+ 1 idx) :below (min (length all-cards)
                                                           (+ 1 idx n-matches))
                  :do (incf (aref each-card-times idx-2) (aref each-card-times idx)))
        :finally (return (reduce #'+ each-card-times))))
