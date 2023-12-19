(defsystem #:aoc-2023
  :class :package-inferred-system
  :name "aoc-2023"
  :author "Christoph Göttschkes"
  :license "MIT"
  :depends-on #.(append
                 (if (uiop:directory-exists-p (merge-pathnames "inputs" *default-pathname-defaults*))
                     (list "aoc-2023/inputs/package")
                     (list "aoc-2023/examples/package"))
                 (list "aoc-2023/src/package")))

(if (uiop:directory-exists-p (merge-pathnames "inputs" *default-pathname-defaults*))
    (register-system-packages "aoc-2023/inputs/package" '(:aoc-2023-data))
    (register-system-packages "aoc-2023/examples/package" '(:aoc-2023-data)))
(register-system-packages "aoc-2023/src/package" '(:aoc-2023))
