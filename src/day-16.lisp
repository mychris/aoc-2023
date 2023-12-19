(defpackage #:aoc-2023/src/day-16
  (:use #:cl)
  (:export #:the-floor-will-be-lava-1
           #:the-floor-will-be-lava-2)
  (:import-from #:aoc-2023-data
                #:*day-16-input*))

(in-package #:aoc-2023/src/day-16)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 16: The Floor Will Be Lava")

(defun parse (stream)
  (loop :with result = (make-array 0 :fill-pointer 0 :adjustable t)
        :for line = (read-line stream nil nil)
        :while line
        :do (vector-push-extend line result)
        :finally (return result)))

(defun mirror (mirror direction width)
  (case mirror
    (#\. direction)
    (#\| (if (= width (abs direction))
             direction
             (list width (- width))))
    (#\- (if (= 1 (abs direction))
             direction
             (list 1 -1)))
    (#\/ (cond ((= 1 direction) (- width))
               ((= -1 direction) width)
               ((= width direction) -1)
               ((= (- width) direction) 1)
               (t (error "Unknown direction ~A" direction))))
    (#\\ (cond ((= 1 direction) width)
               ((= -1 direction) (- width))
               ((= width direction) 1)
               ((= (- width) direction) -1)
               (t (error "Unknown direction ~A" direction))))
    (t (error "Unknown mirror ~A" mirror))))

(defun end (pos direction width height)
  (cond ((= direction 1)
         (= (mod pos width) (1- width)))
        ((= direction -1)
         (= (mod pos width) 0))
        ((= direction width)
         (= (truncate pos width) (1- height)))
        ((= direction (- width))
         (= (truncate pos width) 0))
        (t (error "Unknown direction ~A" direction))))

(defun run-beam (matrix width height pos direction output)
  (loop :for mirror = (aref (aref matrix (truncate pos width)) (mod pos width))
        :for next-direction = (mirror mirror direction width)
        :when (find direction (aref (aref output (truncate pos width)) (mod pos width)))
          :return output
        :do (push direction (aref (aref output (truncate pos width)) (mod pos width)))
        :when (listp next-direction)
          :do (loop :for dir :in next-direction
                    :unless (end pos dir width height)
                      :do (run-beam matrix width height (+ pos dir) dir output))
          :and :return output
        :when (end pos next-direction width height)
          :return output
        :do (setq pos (+ pos next-direction))
            (setq direction next-direction)))

(defun count-energized (matrix pos direction)
  (let* ((width (length (aref matrix 0)))
         (height (length matrix))
         (output (make-array height)))
    (loop :for x :upfrom 0 :below height :do (setf (aref output x) (make-array width :initial-element nil)))
    (run-beam matrix width height pos direction output)
    (loop :for line :across output
          :sum (loop :for p :across line
                     :count p))))

(defun the-floor-will-be-lava-1 (&optional (stream (make-string-input-stream *day-16-input*)))
  (count-energized (parse stream) 0 +1))

(defun the-floor-will-be-lava-2 (&optional (stream (make-string-input-stream *day-16-input*)))
  (let* ((matrix (parse stream))
         (width (length (aref matrix 0)))
         (height (length matrix)))
    (max (loop :for x :upfrom 0 :below width
               :maximize (max (count-energized matrix x width)
                              (count-energized matrix (- (* width height) 1 x) (- width))))
         (loop :for y :upfrom 0 :below height
               :maximize (max (count-energized matrix (* y width) 1)
                              (count-energized matrix (1- (* (1+ y) width)) -1))))))
