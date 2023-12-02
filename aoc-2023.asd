;;;; -*- mode: lisp; coding: utf-8 -*-
;;;; system goingto goingto.asd
;;;;

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
               (:file "d2")))
