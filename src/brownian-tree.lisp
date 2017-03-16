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
   (branches :accessor bt-branches
             :initform nil)))

(defclass bt-branch ()
  ((color :accessor bt-branch-color
          :initarg :color
          :initform "black")
   (points :accessor bt-branch-points
           :initarg :points)))

(defmethod initialize-instance :after ((tree brownian-tree) &rest args)
  args
  (setf (bt-buffer tree) (make-array (list (bt-width tree)
                                           (bt-height tree))
                                     :element-type 'bit
                                     :initial-element 0)))

(defun draw-tree (btree filename)
  (let* ((width (bt-width btree))
         (height (bt-height btree))
         (background-color (bt-bg-color btree))
         (image (make-instance 'svg:svg-image 
                               :image-header (list (cons width height))
                               :bg-color background-color)))
    (dolist (b (bt-branches btree))
      (push (make-instance 'svg:svg-path 
                           :points (bt-branch-points b)
                           :color (svg:random-color)) (svg:svg-objects image)))
    (svg:write-svg-to-file filename (svg:draw image))))

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
  "return a random point in the range of the tree"
  (cons (random (bt-width tree))
        (random (bt-height tree))))

(defun update-point (c tree)
  "update a point C to the tree T"
  (setf (aref (bt-buffer tree)(car c) (cdr c)) 1))

(defun coverage (tree)
  "points which are covered by the TREE"
  (let ((result 0))
    (do ((x 0 (1+ x)))
        ((>= x (bt-width tree)))
      (do ((y 0 (1+ y)))
          ((>= y (bt-height tree)))
        (if (eql 1 (aref (bt-buffer tree) x y)) (incf result))))
    result))

(defun new-particle (tree)
  "introduce a particle and if it meets the tree T, draw the walk else discard the particle"
  (let* ((c (random-point tree))
         (walk nil)
         (initial-point c))
    (loop 
       :while (and (in-bounds-p c tree)
                   (not (on-tree c tree)))
       :do (push c walk)
       :do (setq c (random-step c)))
    (cond ((in-bounds-p c tree)
           (format t "found a walk: ~A length~%" (length walk))
           (loop :for i :in walk :do (update-point i tree))
           (push (make-instance 'bt-branch :points walk) (bt-branches tree))
           t)
          (t 
           (format t "out of bounds: ~A steps, from ~A to ~A.~%"
                   (length walk) initial-point c)
           nil))))

(defun create-tree (&key initial-seed num-branches)
  (let ((ct (make-instance 'brownian-tree)))
    (update-point initial-seed ct)
    (dotimes (i num-branches)
      (loop :until (new-particle ct)))
    ct))
