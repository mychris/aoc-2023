(defpackage #:aoc-2023/src/day-14
  (:use #:cl)
  (:export #:parabolic-reflector-dish-1
           #:parabolic-reflector-dish-2)
  (:import-from #:aoc-2023-data
                #:*day-14-input*))

(in-package #:aoc-2023/src/day-14)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 14: Parabolic Reflector Dish")

(defun parse-string (stream)
  (loop :with result = (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character)
        :with width = 0
        :with height = 0
        :for line = (read-line stream nil nil)
        :while line
        :do (loop :for chr :across line
                  :do (vector-push-extend chr result))
            (incf height)
            (setq width (length line))
        :finally (return (values result width height))))

(defmacro access (matrix row col width)
  `(aref ,matrix (+ (* ,row ,width) ,col)))

(defun tilt-north (matrix width height)
  (loop :for col :upfrom 0 :below width
        :do (loop :with last-support = 0
                  :for row :upfrom 0 :below height
                  :when (char= #\# (access matrix row col width))
                    :do (setq last-support (1+ row))
                  :when (char= #\O (access matrix row col width))
                    :do (setf (access matrix row col width) #\.)
                        (setf (access matrix last-support col width) #\O)
                        (incf last-support))
        :finally (return matrix)))

(defun tilt-south (matrix width height)
  (loop :for col :upfrom 0 :below width
        :do (loop :with last-support = (1- height)
                  :for row :downfrom (1- height) :to 0
                  :when (char= #\# (access matrix row col width))
                    :do (setq last-support (1- row))
                  :when (char= #\O (access matrix row col width))
                    :do (setf (access matrix row col width) #\.)
                        (setf (access matrix last-support col width) #\O)
                        (decf last-support))
        :finally (return matrix)))

(defun tilt-west (matrix width height)
  (loop :for row :upfrom 0 :below height
        :do (loop :with last-support = 0
                  :for col :upfrom 0 :below width
                  :when (char= #\# (access matrix row col width))
                    :do (setq last-support (1+ col))
                  :when (char= #\O (access matrix row col width))
                    :do (setf (access matrix row col width) #\.)
                        (setf (access matrix row last-support width) #\O)
                        (incf last-support))
        :finally (return matrix)))

(defun tilt-east (matrix width height)
  (loop :for row :upfrom 0 :below height
        :do (loop :with last-support = (1- width)
                  :for col :downfrom (1- width) :to 0
                  :when (char= #\# (access matrix row col width))
                    :do (setq last-support (1- col))
                  :when (char= #\O (access matrix row col width))
                    :do (setf (access matrix row col width) #\.)
                        (setf (access matrix row last-support width) #\O)
                        (decf last-support))
        :finally (return matrix)))

(defun tilt-cycle (matrix width height)
  (tilt-east (tilt-south (tilt-west (tilt-north matrix width height) width height) width height) width height))

(defun load-north (matrix width height)
  (loop :for col :upfrom 0 :below width
        :sum (loop :for row :upfrom 0 :below height
                   :when (char= #\O (access matrix row col width))
                     :sum (- height row))))

(defun parabolic-reflector-dish-1 (&optional (stream (make-string-input-stream *day-14-input*)))
  (multiple-value-bind (matrix width height)
      (parse-string stream)
    (load-north (tilt-north matrix width height) width height)))

(defun parabolic-reflector-dish-2 (&optional (stream (make-string-input-stream *day-14-input*)))
  (multiple-value-bind (matrix width height)
      (parse-string stream)
    (let ((cache nil)
          (target-tilt-cycles 1000000000))
      (loop :for x :upfrom 1 :to target-tilt-cycles
            :do (setq matrix (tilt-cycle matrix width height))
            :when (find matrix cache :key #'car :test #'string=)
              :do (let* ((remaining-cycles (- target-tilt-cycles x))
                         (cache-entry (find matrix cache :key #'car :test #'string=))
                         (cycle-start (cdr cache-entry))
                         (cycle-length (abs (- x (cdr cache-entry))))
                         (tilt-cycles-to-do (nth-value 1 (truncate remaining-cycles cycle-length))))
                    (setq matrix (car (nth (- (length cache)
                                              (+ cycle-start tilt-cycles-to-do))
                                           cache))))
              :and :return (load-north matrix width height)
            :do
               (setq cache (push (cons (copy-seq matrix) x) cache))))))
