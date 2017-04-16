;;; tools to write a SVG image file.
(defpackage :svg
  (:use :cl
        :svg.object
        :svg.path
        :svg.polygon
        :svg.circle
        :svg.ellipse)
  (:export :random-color
           :write-svg-to-file
	   :objects
	   ))
(in-package :svg)

;; How do i re-export svg.* files just through (:use :svg)????
;; What format for numbers does svg use??
;; lisp outputs numbers like 0.000d0 etc.. are these valid values??
;; image comes out fine i suppose, so does it matter?

(defparameter svg-namespace
  "http://www.w3.org/2000/svg")
(defparameter svg-public-identifier
  "PUBLIC \"-//W3C//DTD SVG 1.1//EN\"")
(defparameter svg-system-identifier
  "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd")

;; only change if svg needs to be revamped.
(defparameter my-svg-xml-header "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>")
(defparameter my-svg-doc-type 
  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\" >")

;; partition this into a new subfile svg.color
(defun random-color ()
  (nth (random 129) '(:aliceblue  :aqua :aquamarine :azure :beige :bisque :black :blanchedalmond :blue :blueviolet :brown :burlywood :cadetblue :chartreuse :chocolate :coral :cornflowerblue :cornsilk :crimson :cyan :darkblue :darkcyan :darkgoldenrod :darkgray :darkgreen :darkkhaki :darkmagenta :darkolivegreen :darkorange :darkorchid :darkred :darksalmon :darkseagreen :darkslateblue :darkslategray :darkturquoise :darkviolet :deeppink :deepskyblue :dodgerblue :firebrick  :forestgreen :fuchsia :gainsboro  :gold :goldenrod  :green :greenyellow :honeydew :hotpink :indianred :indigo :khaki :lavender :lavenderblush :lawngreen :lemonchiffon :lightblue :lightcyan :lightgoldenrodyellow  :lightgreen :lightpink :lightsalmon :lightseagreen :lightskyblue :lightslategray :lightsteelblue :lightyellow :lime :limegreen :linen :magenta :maroon :mediumaquamarine :mediumblue :mediumorchid :mediumpurple :mediumseagreen :mediumslateblue :mediumspringgreen :mediumturquoise :mediumvioletred :midnightblue :mintcream :mistyrose :moccasin :navy :oldlace :olive :olivedrab :orange :orangered :orchid :palegoldenrod :palegreen :paleturquoise :palevioletred :papayawhip :peachpuff :peru :pink :plum :powderblue :purple :rebeccapurple :red :rosybrown :royalblue :saddlebrown :salmon :sandybrown :seagreen :seashell :sienna :silver :skyblue :slateblue :slategray  :springgreen :steelblue :tan :teal :thistle :tomato :turquoise :violet :wheat :yellow :yellowgreen)))

(defclass svg-image ()
  ((xml-header :accessor svg-xml-header
               :initform my-svg-xml-header)
   (doc-type :accessor svg-doc-type
             :initform my-svg-doc-type)
;; dimensions of the image file and other attrnibutes
   (image-size :accessor image-size
               :initarg :image-size
               :initform (cons 0 0))
;; back ground color of the image
   (bg-color :accessor bg-color
             :initarg :bg-color
             :initform "white")
   (objects :accessor objects
            :initarg :objects
            :initform nil)))

(defmethod draw-svg-image ((img svg-image))
  "convert svg-image object into a list of tags"
  (let* ((x-size (car (image-size img)))
         (y-size (cdr (image-size img)))
         (svg-start-tag (format nil "<svg width=\"~A\" height=\"~A\" viewBox=\"0 0 ~A ~A\" >"
                                x-size y-size
                                x-size y-size))
        (svg-end-tag "</svg>"))
    (list (svg-xml-header img) 
          (svg-doc-type img)
          svg-start-tag
          (mapcar #'draw (objects img))
          svg-end-tag)))

(defun write-svg-to-file (filename obj)
  (with-open-file (svg-stream filename
                              :direction :output
                              :if-exists :supersede)
    (labels ((printer (item-list indentation)
               (dolist (item item-list)
                 (cond ((atom item) (write-line (format nil "~A~A"
                                                        (make-string indentation
                                                                     :initial-element #\Space)
                                                        item)
                                                svg-stream))
                       ((listp item) (printer item (1+ indentation)))))))
      (printer (draw-svg-image obj) 0))))
