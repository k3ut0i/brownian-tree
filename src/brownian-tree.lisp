(defpackage :brownian-tree
  (:use :cl
        :svg)
  (:nicknames :bt)
  (:export :new-particle
           :brownian-tree
           :update-point
           :draw-tree))

(in-package :brownian-tree)

(defclass brownian-tree ()
  ((buffer :accessor bt-buffer
           :initarg :buffer)
   (width :accessor bt-width
          :initarg :width
          :initform 1000)
   (height :accessor bt-height
           :initarg :height
           :initform 1000)
   (bg-color :accessor bt-bg-color
             :initarg :bg-color
             :initform "black" )
   (size :accessor bt-size
         :initform 0)))

(defmethod initialize-instance :after ((tree brownian-tree) &rest args)
  args
  (setf (bt-buffer tree) (make-array (list (bt-width tree)
                                           (bt-height tree))
                                     :element-type 'number
                                     :initial-element 0)))

(defun in-bounds-p (c tree)
  "if the point C is out of our range"
  (let ((x (car c))
        (y (cdr c))
        (x-max (bt-width tree))
        (y-max (bt-height tree)))
    (and (and (> x 0)
              (< x x-max))
         (and (> y 0)
              (< y y-max)))))

;; does the particle touch the tree?
(defun on-tree (c tree)
  "is the point C on the tree T"
  (let ((x (car c))
        (y (cdr c)))
    (not (zerop (aref (bt-buffer tree) x y)))))

;; the random step can be any position on the grid around a co-ordinate.
(defun random-step (c)
  "given a position C, a dotted pair, return the position of a random step"
  (let ((x (car c))
        (y (cdr c)))
    (cons (+ (1- x) (random 3))
          (+ (1- y) (random 3)))))

(defun random-point (tree)
  "return a random point in the range of the tree and not on the tree."
  (let ((x (cons (random (bt-width tree))
                 (random (bt-height tree)))))
    (if (on-tree x tree)
        (random-point tree)
        x)))

(defun update-point (c tree)
  "update a point C to the tree T"
  (incf (bt-size tree))
  (setf (aref (bt-buffer tree)(car c) (cdr c)) (bt-size tree)))

;; mainly for debugging.
(defun coverage (tree)
  "points which are covered by the TREE"
  (let ((result 0))
    (do ((x 0 (1+ x)))
        ((>= x (bt-width tree)))
      (do ((y 0 (1+ y)))
          ((>= y (bt-height tree)))
        (if (> (aref (bt-buffer tree) x y) 0) (incf result))))
    result))

;; TODO
(defun new-particle (tree)
  "introduce a particle and if it meets the tree T, update tree T else discard the particle"
  (let* ((c (random-point tree))
         (prev-c nil))
    (loop 
       :while (and (in-bounds-p c tree)
                   (not (on-tree c tree)))
       :do (setq prev-c c)
       :do (setq c (random-step c)))
    (cond ((in-bounds-p c tree)
           (format t ".")
           (update-point prev-c tree)
           prev-c)
          (t nil))))

(defun create-new-tree (&optional 
			  (size '(1000 1000 2000))
			  (initial-seed '((500 . 500)))
			  (bg-color "white"))
  (let ((x (make-instance 'brownian-tree 
                          :width (first size)
                          :height (second size)
                          :bg-color bg-color))
        (n (third size)))
    (dolist (e initial-seed) (update-point e x))
    (dotimes (i n)
      (loop :until (new-particle x)))
    x))
