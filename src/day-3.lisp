(defpackage #:aoc-2023/day-3
  (:use #:cl)
  (:export #:gear-ratios-1
           #:gear-ratios-2)
  (:import-from #:aoc-2023/day-3-input
                #:*input*))

(in-package #:aoc-2023/day-3)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 3: Gear Ratios")

(defun parse-schematic (stream)
  (let ((width 0)
        (height 0)
        (content (make-array 32 :fill-pointer 0 :adjustable t :element-type 'character)))
    (loop :for line = (read-line stream nil nil)
          :while line
          :do (incf height)
              (when (= 0 width)
                (setf width (+ 2 (length line)))
                (incf height)
                (loop :for x :from 0 :below width :do (vector-push-extend #\. content)))
              (loop :for chr :across (format nil ".~A." line)
                    :do (vector-push-extend chr content)))
    (loop :for x :from 0 :below width :do (vector-push-extend #\. content))
    (values width height content)))

(defun around-positions (width height pos)
  (declare (ignore height))
  (list (1- pos) (- pos 1 width) (- pos 1 (- width))
        (1+ pos) (+ pos 1 (- width)) (+ pos 1 width)
        (- pos width) (+ pos width)))

(defun number-at-position (schematic pos)
  (if (digit-char-p (aref schematic pos))
      (number-at-position schematic (1- pos))
      (cons (1+ pos)
            (reduce (lambda (left right) (+ (* 10 left) right))
                    (loop :while (digit-char-p (aref schematic (1+ pos)))
                          :collect (digit-char-p (aref schematic (incf pos))))))))

(defun numbers-around-symbol (schematic width height pos)
  (map 'list #'cdr (loop :with result = nil
                         :for around-pos :in (around-positions width height pos)
                         :do (when (digit-char-p (aref schematic around-pos))
                               (let ((num (number-at-position schematic around-pos)))
                                 (pushnew num result :key #'car)))
                         :finally (return result))))

(defun map-symbols (fun schematic width height)
  (loop :for pos :upfrom 0 :below (length schematic)
        :if (and (char/= #\. (aref schematic pos))
                 (not (digit-char-p (aref schematic pos))))
          :collect (funcall fun (aref schematic pos) (numbers-around-symbol schematic width height pos))))

(defun sum-parts-from (fun stream)
  (multiple-value-bind (width height schematic)
      (parse-schematic stream)
    (reduce #'+ (funcall fun schematic width height))))

(defun gear-ratios-1 (&optional stream)
  "509115"
  (sum-parts-from (lambda (schematic width height)
                    (map-symbols (lambda (symbol numbers-around)
                                   (declare (ignore symbol))
                                   (reduce #'+ numbers-around))
                                 schematic width height))
                  (or stream (make-string-input-stream *input*))))

(defun gear-ratios-2 (&optional stream)
  "75220503"
  (sum-parts-from (lambda (schematic width height)
                    (map-symbols (lambda (symbol numbers-around)
                                   (if (or (char/= #\* symbol)
                                           (/= 2 (length numbers-around)))
                                         0
                                         (* (nth 0 numbers-around) (nth 1 numbers-around))))
                                 schematic width height))
                  (or stream (make-string-input-stream *input*))))
