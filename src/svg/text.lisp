(defpackage :svg.text
  (:use :cl
        :svg.object)
  (:export :text))

(in-package :svg.text)

(defclass text (object)
  ((start-tag :accessor start-tag
              :initform "<text")
   (end-tag :accessor end-tag
            :initform ">")
   (end-text-tag :accessor end-text-tag
                 :initform "</text>")
   (points-tag :accessor points-tag)
   (content :accessor content
            :initarg :content)
   (text-pos :accessor text-pos
             :initarg :text-pos)
   (fill-color :accessor fill-color ; text fill is === path stroke
               :initarg :fill-color
               :initform "black")
   (fill-tag :accessor fill-tag)))

;;TODO: need to include other attributes
;; font, font-family, size, size vs image pixel sizes.
(defmethod initialize-instance :after ((obj text) &rest args)
  (setf (points-tag obj) (format nil "x=\"~A\" y=\"~A\""
                                 (car (text-pos obj))
                                 (cdr (text-pos obj))))
  (setf (fill-tag obj) (format nil "fill=\"~A\"" (fill-color obj))))

(defmethod svg.object:draw ((obj text))
  (reduce (lambda (a b) (format nil "~A ~A" a b))
          (mapcar (lambda (tag) (funcall tag obj))
                  '(start-tag fill-tag points-tag end-tag content end-text-tag))))
