(defpackage #:aoc-2023/day-25
  (:use #:cl)
  (:export #:snowverload-1))

(in-package #:aoc-2023/day-25)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 25: Snowverload")

(defun parse-list (line)
  (loop :with start = 0
        :while (< start (length line))
        :collect (prog1
                     (subseq line start (or (position #\Space line :start start) (length line)))
                   (setq start (1+ (or (position #\Space line :start start) (length line)))))))

(defun parse (stream)
  (loop :with result = (make-hash-table :test #'equal)
        :for line = (read-line stream nil nil)
        :while line
        :do (let ((key (subseq line 0 (position #\: line)))
                  (connections (parse-list (subseq line (1+ (position #\Space line))))))
              (setf (gethash key result) connections))
        :finally (return result)))

(defun count-components (diagram)
  (loop :with storage = (make-hash-table :test #'equal)
        :for key :being :the :hash-key :in diagram
        :do (setf (gethash key storage) t)
            (loop :for component :in (gethash key diagram)
                  :do (setf (gethash component storage) t))
        :finally (return (hash-table-count storage))))

(defun diagram-to-map (diagram)
  (loop :with result = (make-array (list (count-components diagram) (count-components diagram))
                                   :initial-element 0 :element-type 'fixnum)
        :and name-to-num = (make-hash-table :test #'equal)
        :for key :being :the :hash-key :in diagram
        :do (let ((num (gethash key name-to-num)))
              (when (null num)
                (setq num (setf (gethash key name-to-num) (hash-table-count name-to-num))))
              (loop :for connection :in (gethash key diagram)
                    :do (when (null (gethash connection name-to-num))
                          (setf (gethash connection name-to-num) (hash-table-count name-to-num)))
                        (setf (aref result num (gethash connection name-to-num)) 1)
                        (setf (aref result (gethash connection name-to-num) num) 1)))
        :finally (return result)))

(defun neighbours (graph node)
  (loop :for i :upfrom 0 :below (array-dimension graph 1)
        :when (/= 0 (aref graph node i))
          :collect i))

(defun shortest-path (graph n1 n2)
  (let ((visited (make-array (array-dimension graph 0) :initial-element most-positive-fixnum))
        (neighbour-map (loop :with result = (make-array (array-dimension graph 0))
                             :for node :upfrom 0 :below (array-dimension graph 0)
                             :do (setf (aref result node) (neighbours graph node))
                             :finally (return result)))
        (pred (make-array (array-dimension graph 0)))
        (stack (list n1))
        (best most-positive-fixnum))
    (setf (aref visited n1) 0)
    (loop :for node = (pop stack)
          :while node
          :do (loop :for neighbour :in (aref neighbour-map node)
                    :when (and (> (aref visited neighbour) (1+ (aref visited node)))
                               (> best (1+ (aref visited node))))
                      :do (setf (aref visited neighbour) (1+ (aref visited node)))
                          (setf (aref pred neighbour) node)
                          (if (= neighbour n2)
                              (setq best (min best (aref visited neighbour)))
                              (setq stack (push neighbour stack))))
          :finally (return (loop :with node = n2
                                 :while (/= node n1)
                                 :collect (aref pred node)
                                 :do (setq node (aref pred node)))))))

(defun count-connected (graph node)
  (let ((visited (make-array (array-dimension graph 0) :initial-element nil)))
    (setf (aref visited node) t)
    (loop :with stack = (list node)
          :for n = (pop stack)
          :while n
          :do (loop :for neighbour :in (neighbours graph n)
                    :when (not (aref visited neighbour))
                      :do (setq stack (push neighbour stack))
                    :do (setf (aref visited neighbour) t)))
    (loop :for n :across visited
          :count n)))

(defun solve (graph)
  (let ((edge-count (make-array (array-dimensions graph) :initial-element 0)))
    (loop :for x :upfrom 0 :to 200
          :for left = (random (array-dimension graph 0))
          :for right = (random (array-dimension graph 0))
          :if (/= left right)
            :do (let ((path (shortest-path graph left right)))
                  (loop :for (l r) :on path
                        :while (and l r)
                        :do (incf (aref edge-count (min l r) (max l r))))))
    (let* ((highest-three (subseq (sort (loop :for x :upfrom 0 :below (array-dimension edge-count 0)
                                              :append (loop :for y :upfrom (1+ x) :below (array-dimension edge-count 1)
                                                            :collect (aref edge-count x y)))
                                        #'>)
                                  0 3))
           (removed-edges (loop :for x :upfrom 0 :below (array-dimension edge-count 0)
                                :append (loop :for y :upfrom (1+ x) :below (array-dimension edge-count 1)
                                              :if (find (aref edge-count x y) highest-three)
                                                :collect (cons x y)))))
      (if (< 3 (length removed-edges))
          (solve graph)
          (progn
            (loop :for edge :in removed-edges
                  :do (setf (aref graph (car edge) (cdr edge)) 0)
                      (setf (aref graph (cdr edge) (car edge)) 0))
            (* (count-connected graph (car (nth 0 removed-edges)))
               (count-connected graph (cdr (nth 0 removed-edges)))))))))

(defun snowverload-1 (stream)
  (let* ((wiring-diagram (parse stream))
         (graph (diagram-to-map wiring-diagram)))
    (solve graph)))
