(defpackage #:aoc-2023-system
  (:use #:cl #:asdf))
(in-package #:aoc-2023-system)

(defsystem #:aoc-2023
  :name "aoc-2023"
  :author "Christoph Göttschkes"
  :maintainer "Christoph Göttschkes"
  :license "MIT"
  :depends-on ()
  :serial t
  :pathname #P"src/"
  :components ((:file "package")
               (:file "d1-input")
               (:file "d1")
               (:file "d2-input")
               (:file "d2")
               (:file "d3-input")
               (:file "d3")
               (:file "d4-input")
               (:file "d4")))
