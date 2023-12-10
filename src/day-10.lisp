(defpackage #:aoc-2023/day-10
  (:use #:cl)
  (:export #:pipe-maze-1
           #:pipe-maze-2)
  (:import-from #:aoc-2023/day-10-input
                #:*input*))

(in-package #:aoc-2023/day-10)

(setf (documentation *package* t) "Day 10: Pipe Maze")

(defun parse (stream)
  (loop :with result = (make-array 0 :fill-pointer 0 :adjustable t)
        :and width = 0
        :for line = (read-line stream nil nil)
        :while line
        :do (setq width (length line))
            (loop :for cell :across line :do (vector-push-extend cell result))
        :finally (return (values result width (truncate (length result) width)))))

(defun subst-one-cell (chr pos width)
  (cond ((char= #\S chr) (cons nil nil))
        ((char= #\| chr) (cons (- pos width) (+ pos width)))
        ((char= #\- chr) (cons (1- pos) (1+ pos)))
        ((char= #\L chr) (cons (- pos width) (1+ pos)))
        ((char= #\J chr) (cons (- pos width) (1- pos)))
        ((char= #\7 chr) (cons (1- pos) (+ pos width)))
        ((char= #\F chr) (cons (1+ pos) (+ pos width)))
        ((char= #\. chr) (cons -1 -1))
        (t (error "Invalid char ~A" chr))))

(defun subst-cells-to-new-vec (maze width)
  (loop :with result = (make-array (length maze) :initial-element 0)
        :for idx :upfrom 0 :below (length maze)
        :do (setf (aref result idx) (subst-one-cell (aref maze idx) idx width))
        :finally (return result)))

(defun flood-fill (maze width height start chr predicate)
  (loop :with stack = (list start)
        :for position = (pop stack)
        :while position
        :if (and (<= 0 position (1- (* width height)))
                 (funcall predicate (aref maze position)))
          :do (setf (aref maze position) chr)
              (setq stack (append (list (1+ position) (1- position)
                                        (+ position width) (- position width))
                                  stack))
        :finally (return maze)))

(defun walk-border (maze width height func)
  (loop :for x :upfrom 0 :below width
        :do (funcall func maze width height x)
            (funcall func maze width height (+ x (* width (1- height))))
            (funcall func maze width height (* x width))
            (funcall func maze width height (+ (* x width) (1- width)))
        :finally (return maze)))

(defun walk-maze-loop (maze from-pos)
  (loop :with start-pos = from-pos
        :and to-pos = (car (aref maze from-pos))
        :if (and (/= from-pos (car (aref maze to-pos)))
                 (/= from-pos (cdr (aref maze to-pos))))
          :return nil
        :collect (shiftf from-pos to-pos
                         (if (= from-pos (car (aref maze to-pos)))
                             (cdr (aref maze to-pos))
                             (car (aref maze to-pos))))
        :until (= from-pos start-pos)))

(defun find-loop (maze width height)
  (declare (ignore height))
  (loop :with start-pos = (position #\S maze)
        :with subst-maze = (subst-cells-to-new-vec maze width)
        :for start-tile :in '(#\- #\| #\F #\J #\L #\7)
        :do (setf (aref maze start-pos) start-tile)
            (setf (aref subst-maze start-pos) (subst-one-cell start-tile start-pos width))
            (let ((walk (walk-maze-loop subst-maze start-pos)))
              (when walk (return walk)))
        :finally (error "No loop found")))

(defun stretch-maze (maze width height &key (initial-element nil))
  (loop :with new-width = (* 2 width)
        :with new-height = (* 2 height)
        :with result = (make-array (* new-width new-height) :initial-element initial-element)
        :for row :upfrom 0 :below height
        :do (loop :for col :upfrom 0 :below width
                  :for old-pos = (+ col (* width row))
                  :for new-pos = (+ (* 2 col) (* new-width (* 2 row)))
                  :do (cond ((char= (aref maze old-pos) #\|)
                             (setf (aref result new-pos) #\|)
                             (setf (aref result (+ new-width new-pos)) #\|))
                            ((char= (aref maze old-pos) #\-)
                             (setf (aref result new-pos) #\-)
                             (setf (aref result (1+ new-pos)) #\-))
                            ((char= (aref maze old-pos) #\L)
                             (setf (aref result new-pos) #\L)
                             (setf (aref result (1+ new-pos)) #\-))
                            ((char= (aref maze old-pos) #\J)
                             (setf (aref result new-pos) #\J))
                            ((char= (aref maze old-pos) #\7)
                             (setf (aref result new-pos) #\7)
                             (setf (aref result (+ new-width new-pos)) #\|))
                            ((char= (aref maze old-pos) #\F)
                             (setf (aref result new-pos) #\F)
                             (setf (aref result (1+ new-pos)) #\-)
                             (setf (aref result (+ new-width new-pos)) #\|))))
        :finally (return result)))

(defun shrink-maze (maze width height)
  (loop :with new-width = (/ width 2)
        :with new-height = (/ height 2)
        :with result = (make-array (* new-width new-height) :initial-element #\.)
        :for row :upfrom 0 :below new-height
        :do (loop :for col :upfrom 0 :below new-width
                  :for old-pos = (+ (* 2 col) (* width (* 2 row)))
                  :for new-pos = (+ col (* new-width row))
                  :do (setf (aref result new-pos) (aref maze old-pos)))
        :finally (return result)))

(defun pipe-maze-1 (&optional (stream (make-string-input-stream *input*)))
  "7145"
  (/ (length (multiple-value-call #'find-loop (parse stream))) 2))

(defun pipe-maze-2 (&optional (stream (make-string-input-stream *input*)))
  "445"
  (multiple-value-bind (maze width height) (parse stream)
    (let* ((cells-in-loop-lookup
             (loop :with result = (make-array (length maze) :initial-element nil)
                   :for cell :in (find-loop maze width height)
                   :do (setf (aref result cell) t)
                   :finally (return result)))
           (new-maze-only-loop
             (loop :with result = (make-array (length maze))
                   :for pos :upfrom 0 :below (length maze)
                   :do (setf (aref result pos) (if (aref cells-in-loop-lookup pos)
                                                   (aref maze pos)
                                                   #\.))
                   :finally (return result)))
           (stretched-new-maze-only-loop
             (stretch-maze new-maze-only-loop width height :initial-element #\I)))
      (walk-border stretched-new-maze-only-loop (* 2 width) (* 2 height)
                   (lambda (maze width height pos)
                     (flood-fill maze width height pos #\O (lambda (chr) (char= #\I chr)))))
      (count #\I (shrink-maze stretched-new-maze-only-loop (* 2 width) (* 2 height))))))
