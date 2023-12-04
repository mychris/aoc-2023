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
  (coerce (loop :for raw-line = (read-line stream nil nil)
                :while raw-line
                :for line = (concatenate 'string raw-line " ")
                :for colon = (search ":" line)
                :for bar = (search "|" line)
                :collect (list (read-number-list (subseq line colon bar))
                               (read-number-list (subseq line bar))))
          'vector))

(defun number-of-matches (card)
  (loop :for num :in (nth 1 card) :sum (if (member num (nth 0 card)) 1 0)))

(defun calc-points-of-scratchcard (card)
  (ash 1 (1- (number-of-matches card))))

(defun scratchcards-1 (&optional stream)
  (reduce #'+ (map 'list #'calc-points-of-scratchcard
                   (read-scratchcards (or stream (make-string-input-stream *d4-input*))))))

(defun scratchcards-2 (&optional stream)
  (loop :with all-cards = (read-scratchcards (or stream (make-string-input-stream *d4-input*)))
        :with each-card-times = (make-array (length all-cards) :initial-element 1)
        :for idx :upfrom 0 :below (length all-cards)
        :for n-matches = (number-of-matches (aref all-cards idx))
        :do (loop :for idx-2 :upfrom (+ 1 idx) :below (min (length all-cards)
                                                           (+ 1 idx n-matches))
                  :do (incf (aref each-card-times idx-2) (aref each-card-times idx)))
        :sum (aref each-card-times idx)))
