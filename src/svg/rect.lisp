(defpackage :svg.rect
  (:use :cl
        :svg.object)
  (:export :rect))

(in-package :svg.rect)

(defclass rect (object)
  ((start-tag :accessor start-tag
              :initform "<rect")
   (end-tag :accessor end-tag
            :initform "/>")
   (points-tag :accessor points-tag)
   (small-point :accessor small-point
                :initarg :small-point)
   (width :accessor width
          :initarg :width)
   (height :accessor height
           :initarg :height)))

(defmethod initialize-instance :after ((obj rect) &rest args)
  (setf (points-tag obj) (format nil "x=\"~A\" y=\"~A\" width=\"~A\" height=\"~A\""
                                 (car (small-point obj))
                                 (cdr (small-point obj))
                                 (width obj)
                                 (height obj))))

(defmethod svg.object:draw ((obj rect))
  (reduce (lambda (a b) (format nil "~A ~A" a b))
          (mapcar (lambda (tag) (funcall tag obj))
                  '(start-tag stroke-tag fill-tag points-tag end-tag))))
