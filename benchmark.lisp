(ql:quickload "aoc-2023" :verbose nil :silent t)
(ql:quickload "trivial-benchmark" :verbose nil :silent t)

(defun external-function-symbols (package-designator)
  (loop :for s :being :the :external-symbols :in package-designator
        :if (fboundp s)
          :collect s :into symbols
        :finally (return (sort symbols #'string<))))

(defvar *func* (external-function-symbols (find-package "AOC-2023")))

(unless (listp *func*)
  (setq *func* (list *func*)))

(dolist (func *func*)
  (let ((sym (find-symbol (string-upcase func) (find-package "AOC-2023")))
        (timer (benchmark:make-timer)))
    (when (or (null sym)
              (not (fboundp sym)))
      (error "Can not find ~A" func))
    (dotimes (count 50)
      (benchmark:with-sampling (timer)
        (funcall sym)))
    (princ (format nil "~A~&" func))
    (benchmark:report timer)
    (princ #\Newline)))
