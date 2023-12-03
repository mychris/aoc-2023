(ql:quickload "aoc-2023" :verbose nil :silent t)

(defun external-symbols (package-designator)
  (let (symbols)
    (do-external-symbols (s package-designator)
      (push s symbols))
    (sort symbols #'string<)))

(cl:princ (format nil "~A~&" (documentation (find-package :aoc-2023) t)))

(mapc #'princ
      (loop :for day :upfrom 1
            :for package = (find-package (format nil "AOC-2023.D~A" day))
            :while package
            :collect (let* ((symbols (external-symbols package)))
                       (format nil "~A ~S~&"
                               (documentation package t)
                               (cons (funcall (nth 0 symbols))
                                     (funcall (nth 1 symbols)))))))
