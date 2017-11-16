(defpackage :brownian-tree
  (:use :cl
	;; :svg
	;; :cl-jpeg
	))

(in-package :brownian-tree)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defclass brownian-tree ()
  ((buffer :initarg :buffer
	   :type (simple-array * *))
   (width :initarg :width
	  :type fixnum)
   (height :initarg :height
	   :type fixnum)
   (bg-color :initarg :bg-color
             :initform "black"
	     :type (simple-array * *))
   (attributes :type cons
	       :initform (list :out-of-bounds-count 0
			       :particle-count 0
			       :seed-count 0
			       :max-depth 0)))	  ; Simple a-list for additional info.
  (:documentation "Brownian Tree Object")
  (:default-initargs :width 1000 :height 1000 :bg-color "black"))

(defmethod initialize-instance :after ((tree brownian-tree) &rest args)
  (declare (ignore args))
  (setf (slot-value tree 'buffer)
	(make-array (list (slot-value tree 'width)
			  (slot-value tree 'height))
		    :element-type 'list
		    :initial-element '())))

(defmethod print-object ((tree brownian-tree) stream)
  (declare (type stream stream))
  (let* ((attributes (slot-value tree 'attributes))
	 (width (slot-value tree 'width))
	 (height (slot-value tree 'height))
	 (seeds (getf attributes :seed-count))
	 (particles (getf attributes :particle-count)))
    (format stream "#<BROWNIAN-TREE SIZE:~Ax~A SEEDS:~A PARTICLES:~A >"
	    width height seeds particles)))

(defun in-bounds? (c tree)
  "if the point C is out of our range"
  (declare (type (simple-vector 2) c)
	   (type brownian-tree tree))
  (let ((x (svref c 0))
        (y (svref c 1))
        (x-max (slot-value tree 'width))
        (y-max (slot-value tree 'height)))
    (and (and (>= x 0)
              (< x x-max))
         (and (>= y 0)
              (< y y-max)))))

(defun neighbors (p tree)
  "List the neighbors of a point P."
  (declare (type (simple-vector 2) p)
	   (type brownian-tree tree))
  (let* ((lm (list #(-1 0)
		   #(1 0)
		   #(0 -1)
		   #(0 1)
		   #(1 -1)
		   #(-1 1)
		   #(1 1)
		   #(-1 -1))))
    (flet ((add (v1 v2)
	     (vector (+ (aref v1 0) (aref v2 0))
		     (+ (aref v1 1) (aref v2 1)))))
      (let ((nl  (mapcar (lambda (m) (add m p))
			 lm)))
	(remove-if-not (lambda (p)
			 (in-bounds? p tree))
		       nl)))))

(defun on-tree? (c tree)
  "is the point C on the tree T"
  (declare (type (simple-vector 2) c)
	   (type brownian-tree tree))
  (let ((x (aref c 0))
        (y (aref c 1)))
    (if (aref (slot-value tree 'buffer) x y) t nil)))

(defun touch-tree? (p tree)
  "does the point P touch the TREE."
  (declare (type brownian-tree tree)
	   (type (simple-vector 2) p))
  (some (lambda (p)
	  (on-tree? p tree))
	(neighbors p tree)))

;; Need to fix to not step out of the buffer range. how?
(defun random-step (c)
  "A random step from a given position C for TREE."
  (declare (type (simple-vector 2) c))
  (let* ((x (aref c 0))
	 (y (aref c 1))
	 (n (vector (+ (1- x) (random 3))
		    (+ (1- y) (random 3)))))
    n)) ;; Step gone out of scope

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
    (if (on-tree? x tree)
        (random-point tree)
        x)))

(defun seed (tree lp)
  "Initialize the TREE with a list of point LP."
  (declare (type brownian-tree tree))
  (mapc (lambda (p)
	  (set-point tree p (list :seed
				  (incf (getf (slot-value tree 'attributes)
					      :seed-count))
				  :depth 1)))
	lp))

(defun set-point (tree point attributes)
  "Set the value of POINT with ATTRIBUTES in the TREE."
  (declare (type brownian-tree tree)
	   (type (simple-vector 2) point))
  ;; If  out of bounds we just ignore it
  (let ((in-bounds (in-bounds? point tree))
	(depth (get-depth tree point)))
    (if in-bounds
	(progn (setf (aref (slot-value tree 'buffer)
			   (aref point 0) (aref point 1))
		     attributes)
	       (when (getf attributes :particle)
		 (incf (getf (slot-value tree 'attributes) :particle-count))
		 (setf (getf (aref (slot-value tree 'buffer)
				   (aref point 0) (aref point 1))
			     :depth)
		       depth))
	       (when (< (getf (slot-value tree 'attributes) :max-depth)
		      depth)
		 (setf (getf (slot-value tree 'attributes) :max-depth)
		       depth)))
	(incf (getf (slot-value tree 'attributes) :out-of-bounds-count)))
    in-bounds))

(defun get-depth (tree point)
  (declare (type brownian-tree tree)
	   (type (simple-vector 2) point))
  (let* ((n (neighbors point tree))
	 (depth-list (mapcar (lambda (p)
			       (getf (aref (slot-value tree 'buffer)
					   (svref p 0)
					   (svref p 1)) :depth))
			     n)))
    (1+ (apply #'min
	       (getf (slot-value tree 'attributes) :max-depth)
	       (remove-if #'null depth-list)))))

(defun new-point (tree)
  "Introduce a new point on the TREE."
  (declare (type brownian-tree tree))
  (let ((p (random-point tree))
	(num-steps 0))
    ;;Now move the particle until it reaches the tree
    ;;or goes out of bounds.
    (loop with pos = p
       while (in-bounds? pos tree)
       until (touch-tree? pos tree)
       do (setf pos (random-step pos)
		num-steps (1+ num-steps))
       finally (progn
		 (set-point tree pos (list :particle num-steps))))))

(defun num-points (n tree)
  "Build N new points into the TREE."
  (declare (type brownian-tree tree)
	   (type fixnum n))
  (labels ((particle-count (tree)
	     (getf (slot-value tree 'attributes) :particle-count)))
    (let ((initial-count (particle-count tree)))
      (loop until (= (+ n initial-count) (particle-count tree))
	   do (new-point tree)))))
