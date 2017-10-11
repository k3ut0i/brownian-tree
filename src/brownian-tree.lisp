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
  (let ((x (aref c 0))
        (y (aref c 1))
        (x-max (slot-value tree 'width))
        (y-max (slot-value tree 'height)))
    (and (and (> x 0)
              (< x x-max))
         (and (> y 0)
              (< y y-max)))))

;; does the particle touch the tree?
(defun on-tree? (c tree)
  "is the point C on the tree T"
  (declare (type (simple-vector 2) c)
	   (type brownian-tree tree))
  (let ((x (aref c 0))
        (y (aref c 1)))
    (not (zerop (aref (slot-value tree 'buffer) x y)))))

(defun touch-tree? (p tree)
  "does the point P touch the TREE."
  (declare (type brownian-tree tree)
	   (type (simple-vector 2) p))
  (let ((width (slot-value tree 'width))
	(height (slot-value tree 'height)))
    (do ((i 0 (1+ i)))
	((= i width))
      (do ((j 0 (1+ j)))
	  ((= j height))
	(case (aref (slot-value tree 'buffer) i j)
	  ((:seed) t)
	  ((:new) t)
	  (otherwise nil))))))

;; Need to fix to not step out of the buffer range. how?
(defun random-step (c tree)
  "A random step from a given position C for TREE."
  (declare (type (simple-vector 2) c)
	   (type brownian-tree tree))
  (let* ((x (aref c 0))
	 (y (aref c 1))
	 (n (vector (+ (1- x) (random 3))
		    (+ (1- y) (random 3)))))
    (if (in-bounds? n tree)
	n
	#(-1 -1)))) ;; Step gone out of scope

(defun random-step-no-rev (c p)
  "A random step from the point C but doesn't move back to P."
  (let ((next-step (random-step c)))
    (if (equalp p next-step)
	(random-step-no-rev c p)
	next-step)))

(defun random-point (tree)
  "return a random point in the range of the tree and not on the tree."
  (let ((x (vector (random (slot-value tree 'width))
		   (random (slot-value tree 'height)))))
    (if (on-tree x tree)
        (random-point tree)
        x)))

(defun seed (tree lp)
  "Initialize the TREE with a list of point LP."
  (declare (type brownian-tree tree))
  (mapc (lambda (p)
	  (set-point tree p (list :seed)))
	lp))

(defun set-point (tree point attributes)
  "Set the value of POINT with ATTRIBUTES in the TREE."
  (declare (type brownian-tree tree)
	   (type (simple-vector 2) point))
  (setf (aref tree (aref point 0) (aref point 1))
	attributes))

(defun new-point (tree)
  "Introduce a new point on the TREE."
  (declare (type brownian-tree tree))
  (let ((p (random-point tree)))
    ;;Now move the particle until it reaches the tree
    ;;or goes out of bounds.
    (loop with pos = p
       until (or (touch-tree? pos) (equalp pos #(-1 -1)))
       do (setf pos (random-step tree pos))
       finally (when (touch-tree? pos)
		 (set-point pos (list :new))))))
