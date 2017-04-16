(defpackage :brownian-trails
  (:use :cl
        :svg
	:svg.path
	:svg.text)
  (:nicknames :btrails)
  (:export :new-particle
           :create-btrails
           :brownian-trails
           :update-point
           :draw-brownian-trails))

(in-package :brownian-trails)

(defclass brownian-trails ()
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
             :initform "white" )
   (trails :accessor bt-trails
           :initform nil)))

(defmethod initialize-instance :after ((bt brownian-trails) &rest args)
  args
  (setf (bt-buffer bt) (make-array (list (bt-width bt)
                                                  (bt-height bt))
                                            :element-type 'bit
                                            :initial-element 0)))

(defclass bt-trail ()
  ((color :accessor bt-trail-color
          :initarg :color
          :initform "black")
   (id :accessor bt-trail-id
       :initarg :id
       :initform 0)
   (length :accessor bt-trail-length
           :initarg :length
           :initform 0)
   (points :accessor bt-trail-points
           :initarg :points)))

(defun draw-brownian-trails (bt filename)
  (let* ((width (bt-width bt))
         (height (bt-height bt))
         (background-color (bt-bg-color bt))
         (image (make-instance 'svg:svg-image 
                               :image-size (cons width height)
                               :bg-color background-color)))
    (dolist (b (bt-trails bt))
      (let ((color (svg:random-color)))
        (push (make-instance 'path 
                             :points (bt-trail-points b)
                             :stroke-color color)
              (svg:objects image))

        (push (make-instance 'text
			     :text-pos (cons (* 0.9 width)
					     (* 0.01 height))
			     :content (format nil "id:~A ln:~A"
					      (bt-trail-id b)
					      (bt-trail-length b))
			     :fill-color color)
	      
              (svg:objects image))))
    (write-svg-to-file filename image)))

(defun in-bounds-p (c bt)
  "if the point C is out of our range"
  (let ((x (car c))
        (y (cdr c))
        (x-max (bt-width bt))
        (y-max (bt-height bt)))
    (and (and (> x 0)
              (< x x-max))
         (and (> y 0)
              (< y y-max)))))

(defun on-bt (c bt)
  "is the point C on the bt T"
  (let ((x (car c))
        (y (cdr c)))
    (not (zerop (aref (bt-buffer bt) x y)))))

;; the random step can be any position on the grid around a co-ordinate.
(defun random-step (c)
  "given a position C, a dotted pair, return the position of a random step"
  (let ((x (car c))
        (y (cdr c)))
    (cons (+ (1- x) (random 3))
          (+ (1- y) (random 3)))))

(defun random-point (bt)
  "return a random point in the range of the bt"
  (cons (random (bt-width bt))
        (random (bt-height bt))))

(defun update-point (c bt)
  "update a point C to the bt T"
  (setf (aref (bt-buffer bt)(car c) (cdr c)) 1))

;; mainly for debugging.
(defun coverage (bt)
  "points which are covered by the BT"
  (let ((result 0))
    (do ((x 0 (1+ x)))
        ((>= x (bt-width bt)))
      (do ((y 0 (1+ y)))
          ((>= y (bt-height bt)))
        (if (eql 1 (aref (bt-buffer bt) x y)) (incf result))))
    result))

(defun new-particle (bt)
  "introduce a particle and if it meets the bt T, draw the walk else discard the particle"
  (let* ((c (random-point bt))
         (walk nil)
         (initial-point c))
    (loop 
       :while (and (in-bounds-p c bt)
                   (not (on-bt c bt)))
       :do (push c walk)
       :do (setq c (random-step c)))
    (cond ((in-bounds-p c bt)
           (format t "found a walk: ~A length~%" (length walk))
           (loop :for i :in walk :do (update-point i bt))
           (push (make-instance 'bt-trail 
                                :length (length walk)
                                :points walk 
                                :id (length (bt-trails bt)))
                 (bt-trails bt))
           t)
          (t 
           (format t "out of bounds: ~A steps, from ~A to ~A.~%"
                   (length walk) initial-point c)
           nil))))

(defun create-btrails (&key initial-seed num-trails bg-color size)
  (let ((ct (make-instance 'brownian-trails
			   :bg-color bg-color
			   :width (car size)
			   :height (cdr size))))
    (update-point initial-seed ct)
    (dotimes (i num-trails)
      (loop :until (new-particle ct)))
    ct))
