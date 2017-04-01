(defpackage :svg.polygon
  (:use :cl
        :svg.object))

(in-package :svg.polygon)

(defclass polygon (svg.object:object)
  ((points :accessor points
           :initarg :points
           :initform nil)
   (start-tag :accessor start-tag
              :initform "<polygon")
   (end-tag :accessor end-tag
            :initform  "/>")
   (points-tag :accessor points-tag)))

(defmethod initialize-instance :after ((obj polygon) &rest args)
  (null args)
  (flet ((concat-with-space (a b) (concat-with " " a b))
         (point-to-string (a) (concat-with "," (car a) (cdr a))))
    (setf (points-tag obj) (format nil "points=\"~A\""
                                   (reduce #'concat-with-space
                                           (mapcar #'point-to-string (points obj)))))))
    
(defmethod svg.object.draw ((polygon-object polygon))
  (flet ((concat-with-space (a b) (concat-with " " a b)))
    (reduce #'concat-with-space
            (mapcar (lambda (tag) (funcall tag polygon-object))
                    '(start-tag stroke-tag fill-tag points-tag end-tag)))))

(defun concat-with (connector a b)
  (format nil "~A~A~A" a connector b))
