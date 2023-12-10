(declaim (optimize (speed 3) (debug 0) (safety 0)))

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

(loop :with start-real-time = (get-internal-real-time)
      :and start-run-time = (get-internal-run-time)
      :for year :upfrom 2023
      :for year-package = (find-package (format nil "AOC-~A" year))
      :while year-package
      :do
         (princ (format nil "~A~&" (documentation year-package t)))
         (loop :for package :in (get-packages
                                 (lambda (p) (and (search "AOC" (package-name p))
                                                  (search "DAY" (package-name p))
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
      :finally (princ (format nil "REAL-TIME: ~,3fs~&RUN-TIME:  ~,3fs~&"
                              (/ (- (get-internal-real-time) start-real-time)
                                 internal-time-units-per-second)
                              (/ (- (get-internal-run-time) start-run-time)
                                 internal-time-units-per-second))))
