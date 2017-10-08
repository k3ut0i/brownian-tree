(defpackage :svg.path
  (:use :cl
        :svg.object)
  (:export :path))

(in-package :svg.path)

(defclass path (object)
  ((points :accessor points
           :initarg :points
           :initform nil)
   (start-tag  :accessor start-tag
               :initform "<path")
   (end-tag :accessor end-tag
            :initform "/>")
   (points-tag :accessor points-tag)))

;; TODO: provide options for path types other than just line segments.
;; time saving concat operations needed. just creating the strings is taking too long.
;; maybe i should leave the strings without concatenating them and just print them together
;; to a file.
(defmethod initialize-instance :after ((obj path) &rest args)
  (null args)
  (cond ((eql (length (points obj)) 1)
	 (setf (points-tag obj) (format nil "d=\"M ~A ~A L ~A ~A\""
					(caar (points obj)) (cdar (points obj))
					(caar (points obj)) (cdar (points obj)))))
	((eql (length (points obj)) 0) (setf (points-tag obj) "d=\"\""))
	(t (flet ((point-to-string (p) (format nil "L ~A ~A" (car p) (cdr p))))
	     (let ((head-string  (format nil "M ~A ~A"
					 (caar (points obj))
					 (cdar (points obj))))
		   (tail-string (mapcar #'point-to-string (cdr (points obj)))))
	       (setf (points-tag obj)
		     (format nil
			     "d=\"~{~A~^ ~}\""
			     (cons head-string tail-string))))))))

(defmethod svg.object:draw ((obj path))
  (flet ((concat-with-space (a b) (format nil "~A ~A" a b)))
    (reduce #'concat-with-space
	    (list (start-tag obj)
		  (stroke-tag obj)
		  (fill-tag obj)
		  (points-tag obj)
		  (end-tag obj)))))
