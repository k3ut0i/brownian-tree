(defpackage :brownian-tree
  (:use :cl
;        :svg
	)
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
   (attributes :type cons
	       :initform (list :out-of-bounds-count 0
			       :particle-count 0
			       :seed-count 0)))	  ; Simple a-list for additional info.
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
  (let* ((attributes (slot-value tree 'attributes))
	 (width (slot-value tree 'width))
	 (height (slot-value tree 'height))
	 (seeds (getf attributes :seed-count))
	 (particles (getf attributes :particle-count)))
    (format stream "#<BROWNIAN-TREE SIZE:~Ax~A SEEDS:~A PARTICLES:~A >"
	    width height seeds particles)))

;; Mostly for debugging purposes.
(defun pprint-buffer (stream buffer
		      &optional colon amp (delimiter #\Space))
  "Dump BUFFER data."
  (declare (type (simple-array * *) buffer)
	   (ignore colon amp)) ;; What am I actually ignoring?
  (let* ((dimensions (array-dimensions buffer))
	 (width (car dimensions))
	 (height (cadr dimensions)))
    (dotimes (i width)
      (dotimes (j height)
	(let* ((point (aref buffer i j))
	       (point-char (cond ((getf point :seed) #\S)
				 ((getf point :particle) #\P)
				 (t #\_))))
	  (princ point-char stream))
	(write-char delimiter stream))
      (write-char #\Linefeed stream))))

					;FIXME: Complete this method
(defun draw-tree (tree file-name output-type)
  "Draw the TREE to the output file FILE-NAME of type OUTPUT-TYPE."
  (declare (type brownian-tree tree)
	   (type (simple-array character *) file-name))
  (with-open-file (outf file-name
			:direction :output
			:if-exists :supersede)
    (case output-type
      (:lisp-data (format outf "~A" (slot-value tree 'buffer)))
      (:raw-data (format outf "~/brownian-tree:pprint-buffer/"
			 (slot-value tree 'buffer)))
      (:netpbm-image (draw-netpbm-from-tree tree file-name))
      (:svg-image (draw-svg-from-tree tree file-name)))))

(defun draw-netpbm-from-tree (tree file-name)
  "Draw the image in netpbm format from the TREE to FILE-NAME."
  (declare (type brownian-tree tree)
	   (type (simple-array character *) file-name))
  (with-open-file (outf file-name
			:direction :output
			:if-exists :supersede)
    (let ((image-type "P2")
	  (width (slot-value tree 'width))
	  (height (slot-value tree 'height))
	  (image-depth 255))
      (format outf "~A~%~A ~A~%~A" image-type width height image-depth)
      (pprint-buffer outf (slot-value tree 'buffer)))))

(defun draw-svg-from-tree (tree file-name)
  "Draw svg image from the TREE object to FILE-NAME."
  ;; (declare (type brownian-tree tree)
  ;; 	   (type ((simple-array character *) file-name)))
  (declare (ignore tree file-name))
  '())

(defun in-bounds? (c tree)
  "if the point C is out of our range"
  (let ((x (aref c 0))
        (y (aref c 1))
        (x-max (slot-value tree 'width))
        (y-max (slot-value tree 'height)))
    (and (and (>= x 0)
              (< x x-max))
         (and (>= y 0)
              (< y y-max)))))

(defun neighbors (p tree)
  "List the neighbors of a point P."
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
					      :seed-count)))))
	lp))

(defun set-point (tree point attributes)
  "Set the value of POINT with ATTRIBUTES in the TREE."
  (declare (type brownian-tree tree)
	   (type (simple-vector 2) point))
  ;; If  out of bounds we just ignore it
  (let ((in-bounds (in-bounds? point tree)))
    (if in-bounds
	(progn (setf (aref (slot-value tree 'buffer)
			   (aref point 0) (aref point 1))
		     attributes)
	       (when (getf attributes :particle)
		 (incf (getf (slot-value tree 'attributes) :particle-count))))
	(incf (getf (slot-value tree 'attributes) :out-of-bounds-count)))
    in-bounds))

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
       finally (set-point tree pos (list :particle num-steps)))))
