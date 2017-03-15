(defpackage :brownian-tree
  (:use :cl
        :my-svg)
  (:export :new-particle
           :draw-tree))

(in-package :brownian-tree)

(defconstant horizontal-length 1000)
(defconstant vertical-length 1000)

;; seed the tree
(defparameter *tree* (list (cons 500 500)))

(defun in-bounds-p (c)
  "if the point C is out of our range"
  (let ((x (car c))
        (y (cdr c)))
    (and (and (> x 0)
              (< x horizontal-length))
         (and (> y 0)
              (< y vertical-length)))))

(defun on-tree (c)
  "is the point C on the tree"
  (member c *tree* :test (lambda (l r) (and (eql (car l) (car r))
                                            (eql (cdr l) (cdr r))))))
;; the random step can be any position on the grid around a co-ordinate.
(defun random-step (c)
  "given a position C, a dotted pair, return the position of a random step"
  (let ((x (car c))
        (y (cdr c)))
    (cons (+ (1- x) (random 3))
          (+ (1- y) (random 3)))))

(defun random-point ()
  "return a random point in the range"
  (cons (random horizontal-length)
        (random vertical-length)))

(defun new-particle ()
  "introduce a new particle and if it meets the tree, draw the walk."
  (let* ((c (random-point))
         (walk nil)
         (initial-point c))
    (loop 
       :while (and (in-bounds-p c)
                   (not (on-tree c)))
       :do (push c walk)
       :do (setq c (random-step c)))
    (cond ((in-bounds-p c)
           (setq *tree* (append walk *tree*))
           walk)
          (t 
           (format t "out of bounds: ~A steps, from ~A to ~A.~%"
                   (length walk) initial-point c)
           nil))))

(defun draw-tree (s n)
  "draw a brownian tree given a seed S and number of valid particles N"
  (setq *tree* (list s))
  (let ((tree-elements 1))
    (loop :while (< tree-elements n)
       :when (new-particle)
       :do (push it *tree*)
       :do (incf tree-elements))))
