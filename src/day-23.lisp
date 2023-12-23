(defpackage #:aoc-2023/day-23
  (:use #:cl)
  (:export #:a-long-walk-1
           #:a-long-walk-2))

(in-package #:aoc-2023/day-23)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 23: A Long Walk")

(defstruct node (num) (pos) (dest))

(defun parse (stream)
  (loop :with temp = (make-array 0 :fill-pointer 0 :adjustable t)
        :for line = (read-line stream nil nil)
        :while line
        :do (vector-push-extend line temp)
        :finally (return temp)))

(defun find-entrance (map)
  (cons (position-if (lambda (x) (char/= x #\#)) (aref map 0)) 0))

(defun find-exit (map)
  (cons (position-if (lambda (x) (char/= x #\#)) (aref map (1- (length map)))) (1- (length map))))

(defun is-junction (map x y)
  (<= 3 (+ (if (char/= (aref (aref map (1+ y)) x) #\#) 1 0)
           (if (char/= (aref (aref map y) (1+ x)) #\#) 1 0)
           (if (char/= (aref (aref map (1- y)) x) #\#) 1 0)
           (if (char/= (aref (aref map y) (1- x)) #\#) 1 0))))

(defun other-direction (x-dir y-dir)
  (case y-dir
    (0 (list (cons x-dir 0) (cons 0 1) (cons 0 -1)))
    (t (list (cons 0 y-dir) (cons 1 0) (cons -1 0)))))

(defun can-pass (map x y x-dir y-dir)
  (or (char= (aref (aref map y) x) #\.)
      (and (= 1 x-dir) (char= (aref (aref map y) x) #\>))
      (and (= -1 x-dir) (char= (aref (aref map y) x) #\<))
      (and (= 1 y-dir) (char= (aref (aref map y) x) #\v))
      (and (= -1 y-dir) (char= (aref (aref map y) x) #\^))))

(defun find-all-junctions (map)
  (loop :for x :upfrom 1 :below (1- (length (aref map 0)))
        :append (loop :for y :upfrom 1 :below (1- (length map))
                      :when (and (char/= (aref (aref map y) x) #\#)
                                 (is-junction map x y))
                        :collect (cons x y))))

(defun walk-path-until-junction (map x y x-dir y-dir)
  (if (char= #\# (aref (aref map (+ y y-dir)) (+ x x-dir)))
      nil
      (loop :collect (cons x y) :into result
            :do (incf x x-dir)
                (incf y y-dir)
            :if (or (not (can-pass map x y x-dir y-dir))
                    (is-junction map x y))
              :return (append result (list (cons x y)))
            :else
              :do (loop :for (x-other-dir . y-other-dir) :in (other-direction x-dir y-dir)
                        :when (char/= #\# (aref (aref map (+ y y-other-dir)) (+ x x-other-dir)))
                          :do (setq x-dir x-other-dir)
                              (setq y-dir y-other-dir)
                          :and :return nil))))

(defun create-junction-graph (map)
  (let ((entr (find-entrance map))
        (exit (find-exit map)))
    (setf (aref (aref map (cdr entr)) (car entr)) #\v)
    (setf (aref (aref map (cdr exit)) (car exit)) #\^)
    (let* ((junctions (find-all-junctions map))
           (nodes (loop :for junction :in junctions
                        :collect (make-node :num 0
                                            :pos junction
                                            :dest nil))))
      (push exit junctions)
      (loop :for node :in nodes
            :do (loop :for (x-dir . y-dir) :in '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))
                      :for path = (walk-path-until-junction map (car (node-pos node)) (cdr (node-pos node)) x-dir y-dir)
                      :when path
                        :do (push (cons (1- (length path)) (car (last path))) (node-dest node))))
      (loop :for node :in nodes
            :do (setf (node-dest node) (remove-if-not (lambda (dest) (find (cdr dest) junctions :test #'equal))
                                                      (node-dest node))))
      (push (make-node :pos entr
                       :dest (list
                              (cons (1- (length (walk-path-until-junction map (car entr) (cdr entr) 0 1)))
                                    (car (last (walk-path-until-junction map (car entr) (cdr entr) 0 1))))))
            nodes)
      (push (make-node :pos exit
                       :dest nil)
            nodes)
      (setf (aref (aref map (cdr entr)) (car entr)) #\.)
      (setf (aref (aref map (cdr exit)) (car exit)) #\.)
      (loop :for node :in nodes
            :for i :upfrom 0
            :do (loop :for dest :in (node-dest node)
                      :do (setf (cdr dest) (find-if (lambda (n) (equal (cdr dest) (node-pos n))) nodes)))
                (setf (node-num node) i))
      nodes)))

(defun find-longest-path (map)
  (let* ((graph (create-junction-graph map))
         (entrance (find-entrance map))
         (exit (find-exit map))
         (entrance-node (find-if (lambda (n) (equal entrance (node-pos n))) graph))
         (exit-node (find-if (lambda (n) (equal exit (node-pos n))) graph))
         (queue (list (list 0 entrance-node (ash 1 (node-num entrance-node)))))
         (result 0))
    (loop :for (path-length node path-bitset) = (pop queue)
          :while node
          :do (loop :for (length . neigh) :in (node-dest node)
                    :when (= 0 (logand (ash 1 (node-num neigh)) path-bitset))
                      :do (push (list (+ path-length length)
                                      neigh
                                      (logior path-bitset (ash 1 (node-num neigh))))
                                queue)
                          (when (eq neigh exit-node)
                            (setq result (max result (+ path-length length))))))
    result))

(defun a-long-walk-1 (stream)
  (let ((map (parse stream)))
    (find-longest-path map)))

(defun a-long-walk-2 (stream)
  (let ((map (parse stream)))
    (loop :for y :upfrom 0 :below (length map)
          :do (loop :for x :upfrom 0 :below (length (aref map y))
                    :when (char/= #\# (aref (aref map y) x))
                      :do (setf (aref (aref map y) x) #\.)))
    (find-longest-path map)))
