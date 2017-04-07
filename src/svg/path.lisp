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
(defmethod initialize-instance :after ((obj path) &rest args)
  (null args)
  (flet ((point-to-string (p) (format nil "L ~A ~A" (car p) (cdr p))))
    (let ((head-string  (format nil "M ~A ~A"
                                         (caar (points obj))
                                         (cdar (points obj))))
          (tail-string (reduce (lambda (a b) (format nil "~A ~A" a b))
                               (mapcar #'point-to-string (cdr (points obj))))))
      (setf (points-tag obj) (format nil "d=\"~A ~A\"" head-string tail-string)))))

(defmethod svg.object:draw ((obj path))
  (flet ((concat-with-space (a b) (format nil "~A ~A" a b)))
    (reduce #'concat-with-space
            (mapcar (lambda (tag) (funcall tag obj))
                    '(start-tag stroke-tag fill-tag points-tag end-tag)))))
