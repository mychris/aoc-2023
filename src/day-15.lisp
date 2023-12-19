(defpackage #:aoc-2023/src/day-15
  (:use #:cl)
  (:export #:lens-library-1
           #:lens-library-2)
  (:import-from #:aoc-2023-data
                #:*day-15-input*))

(in-package #:aoc-2023/src/day-15)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 15: Lens Library")

(defconstant +num-boxes+ 256)

(defmacro parse (stream accum fun)
  (let ((initialization (gensym "initialization"))
        (chr (gensym "chr")))
    `(loop :with ,initialization = (make-array 1024 :element-type 'character :fill-pointer 0)
           :for ,chr = (read-char ,stream nil nil)
           :when (and (null ,chr)
                      (< 0 (length ,initialization)))
             ,accum (funcall ,fun ,initialization)
           :until (null ,chr)
           :when (and (char= #\, ,chr)
                      (< 0 (length ,initialization)))
             ,accum (funcall ,fun ,initialization)
           :if (char= #\, ,chr)
             :do (setf (fill-pointer ,initialization) 0)
           :else
             :when (char/= #\Newline ,chr)
               :do (vector-push ,chr ,initialization))))

(defun hash (str)
  (loop :with result = 0
        :for chr :across str
        :do (setq result (mod (* (+ result (char-code chr))
                                 17)
                              +num-boxes+))
        :finally (return result)))

(defun handle-sequence (seq boxes)
  (let* ((label (subseq seq 0 (or (position #\- seq) (position #\= seq))))
         (lens (parse-integer seq :start (1+ (or (position #\= seq) 0)) :junk-allowed t))
         (hash-value (hash label))
         (box (aref boxes hash-value)))
    (if (null lens)
      (setq box (remove label box :test #'string= :key #'car))
      (let ((old (find label box :test #'string= :key #'car)))
        (if old
            (setf (cdr old) lens)
            (setq box (append box (list (cons label lens)))))))
    (setf (aref boxes hash-value) box)))

(defun lens-library-1 (&optional (stream (make-string-input-stream *day-15-input*)))
  (parse stream :sum #'hash))

(defun lens-library-2 (&optional (stream (make-string-input-stream *day-15-input*)))
  (let ((boxes (make-array +num-boxes+ :initial-element nil)))
    (parse stream :do (lambda (seq) (handle-sequence seq boxes)))
    (loop :for box :across boxes
          :for boxnum :upfrom 1
          :sum (loop :for lens :in box
                     :for slot :upfrom 1
                     :sum (* boxnum slot (cdr lens))))))
