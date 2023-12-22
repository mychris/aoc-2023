(defpackage #:aoc-2023/day-12
  (:use #:cl)
  (:export #:hot-springs-1
           #:hot-springs-2))

(in-package #:aoc-2023/day-12)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 12: Hot Springs")

(defun parse-line (line &optional (copy-times 1))
  (let ((row (subseq line 0 (position #\Space line)))
        (condition-string (subseq line (1+ (position #\Space line)))))
    (let ((row-copied "")
          (condition-string-copied ""))
      (dotimes (x copy-times)
        (setq row-copied (concatenate 'string row-copied "?" row))
        (setq condition-string-copied (concatenate 'string condition-string-copied "," condition-string)))
      (setq row (concatenate 'string (subseq row-copied 1) "."))
      (setq condition-string (subseq condition-string-copied 1)))
    (cons row (loop :with start = 0
                    :with result = (make-array 0 :fill-pointer 0 :adjustable t)
                    :while (< start (length condition-string))
                    :do (multiple-value-bind (num end)
                            (parse-integer condition-string :start start :junk-allowed t)
                          (setq start (1+ end))
                          (when num
                            (vector-push-extend num result)))
                    :finally (return result)))))

(defun parse (stream &optional (copy-times 1))
  (loop :while (peek-char nil stream nil nil) :collect (parse-line (read-line stream) copy-times)))

(defmacro hash-key (row-pos conditions-pos grp-count)
  `(logior (ash ,row-pos 20) (ash ,conditions-pos 10) ,grp-count))

(defun calculate-arrangements-rec (row conditions)
  (let* ((*memo* (make-hash-table :test #'equal)))
    (labels ((inner (row-pos conditions-pos grp-count)
               (when (null (gethash (hash-key row-pos conditions-pos grp-count) *memo*))
                 (setf (gethash (hash-key row-pos conditions-pos grp-count) *memo*)
                       (if (= row-pos (length row))
                           (if (and (= conditions-pos (length conditions)) (= 0 grp-count))
                               1
                               0)
                           (loop :for chr :in (if (char= (char row row-pos) #\?)
                                                  (list #\# #\.)
                                                  (list (char row row-pos)))
                                 :sum (if (char= chr #\#)
                                          (inner (1+ row-pos) conditions-pos (1+ grp-count))
                                          (if (/= 0 grp-count)
                                              (if (and (/= conditions-pos (length conditions))
                                                       (= grp-count (aref conditions conditions-pos)))
                                                  (inner (1+ row-pos) (1+ conditions-pos) 0)
                                                  0)
                                              (inner (1+ row-pos) conditions-pos 0)))))))
               (gethash (hash-key row-pos conditions-pos grp-count) *memo*)))
      (inner 0 0 0))))

(defmacro dot-or-q (chr) `(or (char= #\. ,chr) (char= #\? ,chr)))
(defmacro pound-or-q (chr) `(or (char= #\# ,chr) (char= #\? ,chr)))

(defun calculate-arrangements-dyn (row conditions)
  (let ((cache (make-array `(,(1+ (length row)) ,(+ 2 (length conditions)) ,(+ 2 (length row)))
                           :initial-element 0)))
    (setf (aref cache 0 0 0) 1)
    (dotimes (i (length row))
      (dotimes (j (1+ (length conditions)))
        (dotimes (k (1+ (length row)))
          (let ((cur (aref cache i j k)))
            (when (/= 0 cur)
              (when (and (dot-or-q (char row i))
                         (or (= k 0)
                             (= k (aref conditions (1- j)))))
                (incf (aref cache (1+ i) j 0) cur))
              (when (pound-or-q (char row i))
                (incf (aref cache (1+ i) (+ j (if (= 0 k) 1 0)) (1+ k)) cur)))))))
    (aref cache (length row) (length conditions) 0)))

(defun sum-arrangements (input)
  (loop :for (row . conditions) :in input
        :sum (calculate-arrangements-rec row conditions)))

(defun hot-springs-1 (stream)
  (sum-arrangements (parse stream 1)))

(defun hot-springs-2 (stream)
  (sum-arrangements (parse stream 5)))
