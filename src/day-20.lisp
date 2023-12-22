(defpackage #:aoc-2023/src/day-20
  (:use #:cl)
  (:export #:pulse-propagation-1
           #:pulse-propagation-2)
  (:import-from #:aoc-2023-data
                #:*day-20-input*))

(in-package #:aoc-2023/src/day-20)

(declaim (optimize (speed 0) (debug 3) (safety 3)))
(setf (documentation *package* t) "Day 20: Pulse Propagation")

(defstruct module
  (idx)
  (name)
  (handler)
  (state)
  (connected))

(defmacro high-p (pulse) `(< 0 ,pulse))
(defmacro low-p (pulse) `(> 0 ,pulse))

(defun parse-out (str name-to-index)
  (loop :for pos = 0 :then (1+ (or (position #\, str :start pos) (length str)))
        :while (< pos (length str))
        :collect (let ((name (string-trim
                              '(#\Space)
                              (subseq str pos (or (position #\, str :start pos) (length str))))))
                   (when (null (gethash name name-to-index))
                     (setf (gethash name name-to-index) (hash-table-count name-to-index)))
                   (gethash name name-to-index))))

(defun parse (stream &optional (output-name "rx"))
  (let ((name-to-index (make-hash-table :test #'equal))
        (result (make-array 0 :fill-pointer 0 :adjustable t :initial-element nil))
        (start 0))
    (loop :for line = (read-line stream nil nil)
          :while line
          :do (let ((name (subseq line 0 (position #\Space line)))
                    (out-part (subseq line (position #\Space line
                                                     :start (1+ (position #\Space line)))))
                    (module (make-module)))
                (setf (module-handler module) (case (char line 0)
                                                (#\% #'flip-flop-pulse-handler)
                                                (#\& #'conjunction-pulse-handler)
                                                (t #'broadcast-pulse-handler)))
                (setf (module-connected module) (parse-out out-part name-to-index))
                (when (or (char= (char line 0) #\%)
                          (char= (char line 0) #\&))
                  (setq name (subseq name 1)))
                (setf (module-name module) name)
                (when (null (gethash name name-to-index))
                  (setf (gethash name name-to-index) (hash-table-count name-to-index)))
                (setf (module-idx module) (gethash name name-to-index))
                (when (string= name "broadcaster")
                  (setq start (module-idx module)))
                (loop :while (>= (module-idx module) (length result))
                      :do (vector-push-extend nil result))
                (setf (aref result (module-idx module)) module))
          :finally (return (progn
                             (when (gethash output-name name-to-index)
                               (let ((rx-module (make-module)))
                                 (setf (module-idx rx-module) (gethash output-name name-to-index))
                                 (setf (module-name rx-module) (copy-seq output-name))
                                 (setf (module-handler rx-module) #'rx-pulse-handler)
                                 (setf (module-state rx-module) -1)
                                 (setf (module-connected rx-module) nil)
                                 (setf (aref result (module-idx rx-module)) rx-module)))
                             (values start result))))))

(defun modules-initialize-state (modules)
  (loop :for m1 :across modules
        :when (or (eq (module-handler m1) #'broadcast-pulse-handler)
                  (eq (module-handler m1) #'flip-flop-pulse-handler))
          :do (setf (module-state m1) -1)
        :when (eq (module-handler m1) #'conjunction-pulse-handler)
          :do (setf (module-state m1)
                    (list (loop :for m2 :across modules
                                :count (find (module-idx m1) (module-connected m2)))
                          0
                          (make-array (length modules) :initial-element -1)))))

(defun broadcast-pulse-handler (module input input-from)
  (declare (ignore module)
           (ignore input-from))
  input)

(defun flip-flop-pulse-handler (module input input-from)
  (declare (ignore input-from))
  (if (< 0 input)
      0
      (progn (setf (module-state module) (- 0 (module-state module)))
             (module-state module))))

(defun conjunction-pulse-handler (module input input-from)
  (cond ((and (= 1 input) (= -1 (aref (nth 2 (module-state module)) (module-idx input-from))))
         (incf (cadr (module-state module)))
         (setf (aref (nth 2 (module-state module)) (module-idx input-from)) input))
        ((and (= -1 input) (= 1 (aref (nth 2 (module-state module)) (module-idx input-from))))
         (decf (cadr (module-state module)))
         (setf (aref (nth 2 (module-state module)) (module-idx input-from)) input)))
  (if (= (nth 0 (module-state module))
         (nth 1 (module-state module)))
      -1
      +1))

(defun rx-pulse-handler (module input input-from)
  (declare (ignore input-from))
  (when (> 0 input)
    (setf (module-state module) 1))
  0)

(defun press-button (modules start)
  (let ((queue (list (list (aref modules start) -1 nil)))
        (low-count 0)
        (high-count 0))
    (loop :for (module pulse module-from) = (pop queue)
          :while module
          :if (< 0 pulse)
            :do (incf high-count 1)
          :else
            :do (incf low-count 1)
          :do (let ((out (funcall (module-handler module) module pulse module-from)))
                (when (/= 0 out)
                  (loop :for m2 :in (module-connected module)
                        :do (setq queue (append queue (list (list (aref modules m2) out module))))))))
    (values low-count high-count)))

(defun pulse-propagation-1 (&optional (stream (make-string-input-stream *day-20-input*)))
  (multiple-value-bind (start modules)
      (parse stream)
    (modules-initialize-state modules)
    (let ((lc 0) (hc 0))
      (dotimes (i 1000)
        (multiple-value-bind (low-count high-count)
            (press-button modules start)
          (incf lc low-count)
          (incf hc high-count)))
      (* lc hc))))

(defun pulse-propagation-2 (&optional (stream (make-string-input-stream *day-20-input*)))
  0)
