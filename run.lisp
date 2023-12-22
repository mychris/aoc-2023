(ql:quickload "aoc-2023" :verbose nil :silent t)

(use-package :cl)

(defun get-packages (filter)
  (sort (remove-if-not filter (list-all-packages))
        ;; This is a big hack.  Each package has either 2 digits, or a dash and a digit as its
        ;; suffix.  `parse-integer` will parse the 2 digits as a number, or the dash and the digit
        ;; as a negative number.
        ;; Should be made cleaner at some point (which is probably never).
        (lambda (left right) (< (abs (parse-integer left :start (- (length left) 2)))
                                (abs (parse-integer right :start (- (length right) 2)))))
        :key #'package-name))

(defun external-function-symbols (package-designator)
  (loop :for s :being :the :external-symbols :in package-designator
        :if (fboundp s)
          :collect s :into symbols
        :finally (return (sort symbols #'string<))))

(defun run (fun input-file)
  (if fun
      (with-open-file (stream input-file :direction :input)
        (funcall fun stream))
      nil))

(let ((start-real-time (get-internal-real-time))
      (start-run-time (get-internal-run-time)))
  (princ (format nil "~A~&" (documentation (find-package "AOC-2023") t)))
  (loop :for day :upfrom 1
        :for package = (find-package (format nil "AOC-2023/DAY-~A" day))
        :while package
        :do (let* ((symbols (external-function-symbols package))
                   (f1 (first symbols))
                   (f2 (second symbols))
                   (input-file (asdf:system-relative-pathname
                                "aoc-2023"
                                (format nil "inputs/day-~A.txt" day))))
              (if (not (uiop:file-exists-p input-file))
                  (princ (format nil "~A NO INPUT FOUND~&" (documentation package t)))
                  (let ((r1 (run f1 input-file))
                        (r2 (run f2 input-file)))
                    (princ (format nil "~A ~S~&" (documentation package t) (cons r1 r2)))))))
  (princ (format nil "REAL-TIME: ~,3fs~&RUN-TIME:  ~,3fs~&"
                 (/ (- (get-internal-real-time) start-real-time)
                    internal-time-units-per-second)
                 (/ (- (get-internal-run-time) start-run-time)
                    internal-time-units-per-second))))
