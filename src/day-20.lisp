(defpackage #:aoc-2023/day-20
  (:use #:cl)
  (:export #:pulse-propagation-1
           #:pulse-propagation-2))

(in-package #:aoc-2023/day-20)

(declaim (optimize (speed 0) (debug 3) (safety 3)))
(setf (documentation *package* t) "Day 20: Pulse Propagation")

(defstruct module
  (idx)
  (name)
  (handler)
  (state)
  (out-was-high-once)
  (connected))

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

(defun find-inputs-for (modules name)
  (let ((target-num (loop :for i :upfrom 0 :below (length modules)
                          :when (string= (module-name (aref modules i)) name)
                            :return i)))
    (loop :for i :upfrom 0 :below (length modules)
          :when (find-if (lambda (x) (= x target-num)) (module-connected (aref modules i)))
            :collect i)))

(defun modules-initialize-state (modules)
  (loop :for m1 :across modules
        :do (setf (module-out-was-high-once m1) nil)
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
  (declare (ignore input-from))
  (setf (module-out-was-high-once module) (or (module-out-was-high-once module) (= 1 input)))
  input)

(defun flip-flop-pulse-handler (module input input-from)
  (declare (ignore input-from))
  (let ((out (if (< 0 input)
                 0
                 (progn (setf (module-state module) (- 0 (module-state module)))
                        (module-state module)))))
    (setf (module-out-was-high-once module) (or (module-out-was-high-once module) (= 1 out)))
    out))

(defun conjunction-pulse-handler (module input input-from)
  (cond ((and (= 1 input) (= -1 (aref (nth 2 (module-state module)) (module-idx input-from))))
         (incf (cadr (module-state module)))
         (setf (aref (nth 2 (module-state module)) (module-idx input-from)) input))
        ((and (= -1 input) (= 1 (aref (nth 2 (module-state module)) (module-idx input-from))))
         (decf (cadr (module-state module)))
         (setf (aref (nth 2 (module-state module)) (module-idx input-from)) input)))
  (let ((out (if (= (nth 0 (module-state module))
                    (nth 1 (module-state module)))
                 -1
                 +1)))
    (setf (module-out-was-high-once module) (or (module-out-was-high-once module) (= 1 out)))
    out))

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

(defun count-button-presses-until-high (modules start target)
  (loop :for count :upfrom 0
        :when (module-out-was-high-once target)
          :return count
        :do (press-button modules start)))

(defun find-inputs-for (modules name)
  (let ((target-num (loop :for i :upfrom 0 :below (length modules)
                          :when (string= (module-name (aref modules i)) name)
                            :return i)))
    (loop :for module :across modules
          :when (find-if (lambda (x) (= x target-num)) (module-connected module))
            :collect module)))

(defun pulse-propagation-1 (stream)
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

(defun pulse-propagation-2 (stream)
  (multiple-value-bind (start modules)
      (parse stream)
    (let* ((inputs-of-rx (find-inputs-for modules "rx"))
           (inputs-of-rx-input (find-inputs-for modules (module-name (car inputs-of-rx)))))
      (if (and (= 1 (length inputs-of-rx))
               (every (lambda (mod) (eq (module-handler mod) #'conjunction-pulse-handler)) inputs-of-rx-input))
          (apply #'lcm
                 (loop :for target :in inputs-of-rx-input
                       :do (modules-initialize-state modules)
                       :collect (count-button-presses-until-high modules start target)))
          0))))
