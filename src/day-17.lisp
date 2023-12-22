(defpackage #:aoc-2023/day-17
  (:use #:cl)
  (:export #:clumsy-crucible-1
           #:clumsy-crucible-2))

(in-package #:aoc-2023/day-17)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
(setf (documentation *package* t) "Day 17: Clumsy Crucible")

(defun parse (stream)
  (loop :with map = (make-array 0 :fill-pointer 0 :adjustable t :element-type 'fixnum)
        :with width = 0
        :for line = (read-line stream nil nil)
        :while line
        :do (setq width (length line))
            (loop :for chr :across line
                  :do (vector-push-extend (- (char-code chr) (char-code #\0)) map))
        :finally (return (values map width))))

;; 0 -> right
;; 1 -> up
;; 2 -> left
;; 3 -> down

(defun other-directions (direction)
  (case direction
    (0 '(1 3))
    (1 '(0 2))
    (2 '(1 3))
    (3 '(0 2))))

(defun next-position (map width pos direction)
  (case direction
    (0 (if (/= 0 (mod (1+ pos) width))
           (1+ pos)
           -1))
    (1 (if (< width pos)
           (- pos width)
           -1))
    (2 (if (/= 0 (mod pos width))
           (1- pos)
           -1))
    (3 (if (< pos (- (length map) width))
           (+ pos width)
           -1))))

(defun push-sorted (elem queue &key (key #'identity))
  (cond ((null queue)
         (list (list elem)))
        ((< (funcall key elem) (funcall key (caar queue)))
         (push (list elem) queue))
        ((= (funcall key elem) (funcall key (caar queue)))
         (setf (car queue) (append (list elem) (car queue)))
         queue)
        (t (loop :for x = queue :then (cdr x)
                 :if (null (cdr x))
                   :return (progn
                             (setf (cdr x) (list (list elem)))
                             queue)
                 :if (= (funcall key elem) (funcall key (caadr x)))
                   :return (progn
                             (setf (cadr x) (append (list elem) (cadr x)))
                             queue)
                 :if (< (funcall key elem) (funcall key (caadr x)))
                   :return (progn
                             (setf (cdr x) (cons (list elem) (cdr x)))
                             queue)))))

(defmacro pop-sorted (queue)
  (let ((first-list (gensym "first-list")))
    `(let ((,first-list (pop ,queue)))
       (prog1 (pop ,first-list)
         (when ,first-list
           (push ,first-list ,queue))))))

;; queue is the bottle-neck.  Would be better to use a real priority queue
;; queue is now a sorted queue, each position is a list of elements with the same priority.
(defun find-path (map width start goal min-move max-move)
  (let ((visited (make-array (length map) :initial-element 0 :element-type 'fixnum))
        (queue nil)
        (cost-cache (make-array (* 4 (length map)) :initial-element most-positive-fixnum
                                                   :element-type 'fixnum)))
    (setq queue (push-sorted (list start 0 0) queue :key #'cadr))
    (setq queue (push-sorted (list start 0 1) queue :key #'cadr))
    (setq queue (push-sorted (list start 0 2) queue :key #'cadr))
    (setq queue (push-sorted (list start 0 3) queue :key #'cadr))
    (loop :while queue
          :for (this-pos this-cost this-dir) = (pop-sorted queue)
          :when (= this-pos goal)
            :return this-cost
          :when (= 0 (logand (aref visited this-pos) (ash 1 this-dir)))
            :do (setf (aref visited this-pos) (ash 1 this-dir))
                (loop :for other-direction :in (other-directions this-dir)
                      :for cost-sum = this-cost
                      :do (loop :with other-pos = this-pos
                                :for steps :from 1 :to max-move
                                :do (setq other-pos (next-position map width other-pos other-direction))
                                :when (<= 0 other-pos)
                                  :do (let ((cost-cache-index (+ (* other-pos 4) other-direction)))
                                        (incf cost-sum (aref map other-pos))
                                        (when (<= min-move steps)
                                          (when (< cost-sum (aref cost-cache cost-cache-index))
                                            (setf (aref cost-cache cost-cache-index) cost-sum)
                                            (setq queue
                                                  (push-sorted (list other-pos cost-sum other-direction)
                                                               queue
                                                               :key #'cadr
                                                               ))))))))))

(defun clumsy-crucible-1 (stream)
  (multiple-value-bind (map width)
      (parse stream)
    (find-path map width 0 (1- (length map)) 1 3)))

(defun clumsy-crucible-2 (stream)
  (multiple-value-bind (map width)
      (parse stream)
    (find-path map width 0 (1- (length map)) 4 10)))
