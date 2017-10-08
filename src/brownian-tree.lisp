(defpackage :brownian-tree
  (:use :cl
        :svg)
  (:export :new-particle
           :brownian-tree
           :update-point
           :draw-tree))

(in-package :brownian-tree)

(defclass brownian-tree ()
  ((buffer :initarg :buffer
	   :type (simple-array * *))
   (width :initarg :width
	  :type integer)
   (height :initarg :height
	   :type integer)
   (bg-color :initarg :bg-color
             :initform "black"
	     :type (simple-array * *))
   (size :initform 0))
  (:documentation "Brownian Tree Object")
  (:default-initargs :width 1000 :height 1000 :bg-color "black"))

(defmethod initialize-instance :after ((tree brownian-tree) &rest args)
  (declare (ignore args))
  (setf (slot-value tree 'buffer)
	(make-array (list (slot-value tree 'width)
			  (slot-value tree 'height))
		    :element-type 'number
		    :initial-element 0)))

(defun pprint-buffer (stream buffer
		      &optional colon amp (delimiter #\Space))
  "Dump BUFFER data."
  (declare (type (simple-array integer *) buffer)
	   (ignore colon amp)) ;; What am I actually ignoring?
  (loop :with first-time t
     :for x :across buffer
     :unless first-time
     :do (when delimiter (write-char delimiter stream)) :end
     :do (princ x stream)))

					;FIXME: Complete this method
(defun draw-tree (tree file-name output-type)
  "Draw the TREE to the output file FILE-NAME of type OUTPUT-TYPE."
  (declare (type brownian-tree tree)
	   (type (simple-array character *) file-name))
  (with-open-file (outf file-name
			:direction :output
			:if-exists :supersede)
    (case output-type
      (:lisp-data (format outf "~A" tree))
      (:netpbm-image (format outf "~' :@/pprint-buffer/"
			     (slot-value tree 'buffer)))
      (:svg-image (draw-svg-from-tree tree file-name)))))

(defun draw-svg-from-tree (tree file-name)
  "Draw svg image from the TREE object to FILE-NAME."
  '())

(defun in-bounds? (c tree)
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
(defun on-tree? (c tree)
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
