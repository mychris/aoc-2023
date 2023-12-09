(ql:quickload "aoc-2023" :verbose nil :silent t)

(use-package :cl)

(defun get-packages (filter)
  (sort (remove-if-not filter (list-all-packages))
        #'string<
        :key #'package-name))

(defun external-function-symbols (package-designator)
  (loop :for s :being :the :external-symbols :in package-designator
        :if (fboundp s)
          :collect s :into symbols
        :finally (return (sort symbols #'string<))))

(princ (format nil "~A~&" (documentation (find-package :aoc-2023) t)))

(loop :for package :in (get-packages
                        (lambda (p) (and (search "AOC-2023/DAY" (package-name p))
                                         (not (search "INPUT" (package-name p))))))
      :for symbols = (external-function-symbols package)
      :while package
      :do (let ((r1 (funcall (first symbols)))
                (r2 (funcall (second symbols)))
                (e1 (parse-integer (documentation (first symbols) 'function)))
                (e2 (parse-integer (documentation (second symbols) 'function))))
            (princ (format nil "~A ~S~A~&"
                           (documentation package t)
                           (cons r1 r2)
                           (if (or (/= r1 e1)
                                   (/= r2 e2))
                               " :("
                               "")))))
