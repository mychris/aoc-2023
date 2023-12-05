(defpackage #:aoc-2023/day-5
  (:use #:cl)
  (:export #:if-you-give-a-seed-a-fertilizer-1
           #:if-you-give-a-seed-a-fertilizer-2)
  (:import-from #:aoc-2023/day-5-input
                #:*input*))

(in-package #:aoc-2023/day-5)

(setf (documentation *package* t) "Day 5: If You Give A Seed A Fertilizer")

(defun parse-ints (str)
  (loop :for idx :from 0 :below (length str)
        :append (multiple-value-bind (number end)
                    (parse-integer str :start idx :junk-allowed t)
                  (setq idx end)
                  (if number (list number) number))))

(defun parse-almanac-from (stream)
  (loop :for line = (read-line stream nil nil)
        :while (and line (< 0 (length line)))
        :collect (parse-ints line)))

(defun parse-input-from (stream)
  (loop :for line = (read-line stream nil nil)
        :while line
        :if (search "seeds:" line)
          :collect (parse-ints line)
            :into seeds
        :if (search " map:" line)
          :collect (cons (subseq line 0 (search " map:" line))
                         (sort (parse-almanac-from stream) #'< :key #'cadr))
            :into mappings
        :finally (return (values (car seeds) mappings))))

(defun almanac-map (map-list input)
  (cond
    ((and (typep input 'list) (typep (cdr input) 'list))
     (loop :for value :in input :append (almanac-map map-list value)))
    ((typep input 'cons)
     (loop :with (input-start input-range) = (list (car input) (cdr input))
                 :for (dest-start src-start range) :in map-list
                 :if (and (< input-start src-start)
                          (<= src-start (1- (+ input-start input-range))))
                   :collect (cons input-start (- src-start input-start))
                     :into result
                     :and :do
                       (incf input-start (cdar result))
                       (decf input-range (cdar result))
                 :if (<= src-start input-start (1- (+ src-start range)))
                   :collect (cons (+ dest-start (- input-start src-start))
                                  (min (- range (- input-start src-start)) input-range))
                     :into result
                     :and :do
                       (incf input-start (cdar result))
                       (decf input-range (cdar result))
                 :finally (when (> input-range 0)
                            (setq result (append (list (cons input-start input-range)) result)))
                 :finally (return (sort result #'< :key #'car))))))

(defun almanac-find-mapping-from (from mappings)
  (find-if (lambda (mapping) (= 0 (or (search from (car mapping)) -1))) mappings))

(defun find-min-location (mappings seeds)
  (loop :with from = "seed"
        :until (string= from "location")
        :for this-mapping = (almanac-find-mapping-from from mappings)
        :do (setq from (subseq (car this-mapping)
                               (1+ (position #\- (car this-mapping) :from-end t))))
        :do (setq seeds (almanac-map (cdr this-mapping) seeds)))
  (reduce #'min seeds :key #'car))

(defun if-you-give-a-seed-a-fertilizer-1 (&optional (stream (make-string-input-stream *input*)))
  "157211394"
  (multiple-value-bind (seeds mappings) (parse-input-from stream)
    (find-min-location mappings
                       (loop :for seed :in seeds :collect (cons seed 1)))))

(defun if-you-give-a-seed-a-fertilizer-2 (&optional (stream (make-string-input-stream *input*)))
  "50855035"
  (multiple-value-bind (seeds mappings) (parse-input-from stream)
    (find-min-location mappings
                       (loop :for (start range) :on seeds :by #'cddr :collect (cons start range)))))
