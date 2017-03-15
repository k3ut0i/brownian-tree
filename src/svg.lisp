;;; tools to write a SVG image file.
(defpackage :my-svg
  (:use :cl)
  (:export :svg-image
           :svg-path
           :svg-objects
           :draw
           :write-svg-to-file))
(in-package :my-svg)

(defconstant my-svg-xml-header "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>")
(defconstant my-svg-doc-type 
  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\" >")

(defclass svg-image ()
  ((xml-header :accessor svg-xml-header
               :initarg :xml-header
               :initform my-svg-xml-header)
   (doc-type :accessor svg-doc-type
             :initarg :doc-type
             :initform my-svg-doc-type)
;; dimensions of the image file and other attributes
   (image-header :accessor svg-image-header
                 :initarg :image-header
                 :initform (list (cons 0 0)))
   (objects :accessor svg-objects
            :initarg :objects
            :initform nil))
  )


(defclass svg-object ()
  ((uid :accessor svg-object-uid
        :initarg :uid) ;; string id that will be usefull for debugging svg files.
   (color :accessor svg-object-color
          :initarg :color
          :initform "black")
   (fill :accessor svg-object-fill
         :initarg :fill
         :initform "white")))

;; TODO SVG path uses bezier curves to connect it's points
;; I need a line segment version right now.?
(defclass svg-path (svg-object)
  ((points :accessor path-points
           :initarg :points
           :initform nil)))

(defmethod draw ((a svg-path))
  (labels ((draw-segment (l r)
             (format nil "<path fill=\"node\" stroke=\"black\" d=\"M~A,~AL~A,~A\" />"
                     (car l) (cdr l)
                     (car r) (cdr r))))
    (loop :for i :in (path-points a)
       :with previous = (car (path-points a))
       :collect (draw-segment previous i)
       :do (setq previous i))))


;; TODO: complete the svg-render function.
(defmethod draw ((a svg-image))
  (labels ((svg-render (h o)
             (list (format nil "<svg height=\"~A\" width=\"~A\" >" (car (car h)) (cdr (car h)))
                   (mapcar #'draw o)
                   "</svg>")))
    (list  (svg-xml-header a)
           (svg-doc-type a)
           (svg-render (svg-image-header a) (svg-objects a)))))

(defun write-svg-to-file (filename svg-drawn-image)
  (with-open-file (svg-stream filename
                              :direction :output
                              :if-exists :overwrite)
    (labels ((printer (a i)
               (cond ((null (car a)) t) 
                     ((atom (car a)) (progn
                                       ;; TODO:replace write-line with pprint to include indent
                                       ;; variable
                                       (write-line (car a) svg-stream)
                                       (printer (cdr a) i)))
                     ((listp (car a)) (progn (printer (car a) (1+ i))
                                             (printer (cdr a) i))))))
      (printer svg-drawn-image 0))
    ))
