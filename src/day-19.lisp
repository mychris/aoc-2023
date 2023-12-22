(defpackage #:aoc-2023/day-19
  (:use #:cl)
  (:export #:aplenty-1
           #:aplenty-2))

(in-package #:aoc-2023/day-19)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 19: Aplenty")

(defun condition-to-fun (condition next-workflow)
  (let ((left (subseq condition 0 (position-if-not #'alphanumericp condition)))
        (right (parse-integer (subseq condition (1+ (position-if-not #'alphanumericp condition)))))
        (op (char condition (position-if-not #'alphanumericp condition))))
    (list op left right next-workflow)))

(defun rule-to-fun (rule)
  (if (not (position #\: rule))
      (list nil nil nil rule)
      (condition-to-fun (subseq rule 0 (position #\: rule))
                        (subseq rule (1+ (position #\: rule))))))

(defun split-rules (str)
  (loop :for start = 0
          :then (1+ (or (position #\, str :start start) (length str)))
        :while (< start (length str))
        :collect (subseq str start (or (position #\, str :start start) (length str)))))

(defun split-rating (line)
  (loop :for pos = 0
          :then (1+ (or (position #\, line :start pos) (length line)))
        :while (< pos (length line))
        :collect(let ((part (subseq line pos (position #\= line :start pos)))
                      (rating (subseq line
                                      (1+ (position #\= line :start pos))
                                      (or (position #\, line :start pos) (length line)))))
                  (cons part (parse-integer rating)))))

(defun parse (stream)
  (let ((workflows (loop :for line = (read-line stream nil nil)
                         :while (and line (< 0 (length line)))
                         :collect (let ((name (subseq line 0 (position #\{ line)))
                                        (rules (split-rules (subseq line
                                                                    (1+ (position #\{ line))
                                                                    (position #\} line)))))
                                    (cons name (map 'list #'rule-to-fun rules)))))
        (ratings (loop :for line-raw = (read-line stream nil nil)
                       :while (and line-raw (< 0 (length line-raw)))
                       :collect (split-rating (subseq line-raw 1 (1- (length line-raw)))))))
    (values workflows ratings)))

(defun apply-rule (op left right next-workflow rating)
  (cond ((null op) next-workflow)
        ((char= op #\<) (and (< (cdr (assoc left rating :test #'equal)) right)
                             next-workflow))
        ((char= op #\>) (and (> (cdr (assoc left rating :test #'equal)) right)
                             next-workflow))))

(defun run-workflow (workflows rating)
  (loop :for workflow-name = "in"
          :then (loop :for (op left right next-workflow) :in (cdr (assoc workflow-name workflows :test #'equal))
                      :for rule-result = (apply-rule op left right next-workflow rating)
                      :when rule-result :return rule-result)
        :when (string= workflow-name "A")
          :return t
        :when (string= workflow-name "R")
          :return nil))

(defun split (min-max-rating op left right)
  (cond ((null min-max-rating) nil)
        ((null op) (cons min-max-rating nil))
        (t (let ((low (car (cdr (assoc left min-max-rating :test #'equal))))
                 (high (cdr (cdr (assoc left min-max-rating :test #'equal)))))
             (case op
               (#\< (cond ((< high right) (cons min-max-rating nil))
                          ((>= low right) (cons nil min-max-rating))
                          (t (let ((accept (copy-alist min-max-rating))
                                   (reject (copy-alist min-max-rating)))
                               (setf (cdr (assoc left accept :test #'equal)) (cons low (1- right)))
                               (setf (cdr (assoc left reject :test #'equal)) (cons right high))
                               (cons accept reject)))))
               (#\> (cond ((> low right) (cons min-max-rating nil))
                          ((<= high right) (cons nil min-max-rating))
                          (t (let ((accept (copy-alist min-max-rating))
                                   (reject (copy-alist min-max-rating)))
                               (setf (cdr (assoc left accept :test #'equal)) (cons (1+ right) high))
                               (setf (cdr (assoc left reject :test #'equal)) (cons low right))
                               (cons accept reject))))))))))

(defun numbers-in-range (min-max-rating)
  (loop :with result = 1
        :for elem :in min-max-rating
        :do (setq result (* result (1+ (- (cddr elem) (cadr elem)))))
        :finally (return result)))

(defun find-ranges (min-max-rating workflow-name workflows)
  (cond ((string= workflow-name "R") nil)
        ((string= workflow-name "A") (list min-max-rating))
        (t (loop :for (op left right next-workflow) :in (cdr (assoc workflow-name workflows :test #'equal))
                 :for accept-reject = (split min-max-rating op left right)
                 :while accept-reject
                 :append (find-ranges (car accept-reject) next-workflow workflows)
                 :do (setq min-max-rating (cdr accept-reject))))))

(defun create-min-max-rating (rating min max)
  (map 'list (lambda (elem) (cons (car elem) (cons min max))) rating))

(defun aplenty-1 (stream)
  (multiple-value-bind (workflows ratings)
      (parse stream)
    (loop :for rating :in ratings
          :when (run-workflow workflows rating)
            :sum (loop :for elem :in rating
                       :sum (cdr elem)))))

(defun aplenty-2 (stream)
  (multiple-value-bind (workflows ratings)
      (parse stream)
    (let ((min-max-rating (create-min-max-rating (car ratings) 1 4000)))
      (loop :for range :in (find-ranges min-max-rating "in" workflows)
            :sum (numbers-in-range range)))))
