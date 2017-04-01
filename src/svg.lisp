;;; tools to write a SVG image file.
(defpackage :svg
  (:use :cl)
  (:export :random-color
           :svg-image
           :svg-path
           :svg-circle
           :svg-objects
           :svg-tags
           :draw
           :write-svg-to-file))
(in-package :svg)


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

(defun random-color ()
  (nth (random 129) '(:aliceblue  :aqua :aquamarine :azure :beige :bisque :black :blanchedalmond :blue :blueviolet :brown :burlywood :cadetblue :chartreuse :chocolate :coral :cornflowerblue :cornsilk :crimson :cyan :darkblue :darkcyan :darkgoldenrod :darkgray :darkgreen :darkkhaki :darkmagenta :darkolivegreen :darkorange :darkorchid :darkred :darksalmon :darkseagreen :darkslateblue :darkslategray :darkturquoise :darkviolet :deeppink :deepskyblue :dodgerblue :firebrick  :forestgreen :fuchsia :gainsboro  :gold :goldenrod  :green :greenyellow :honeydew :hotpink :indianred :indigo :khaki :lavender :lavenderblush :lawngreen :lemonchiffon :lightblue :lightcyan :lightgoldenrodyellow  :lightgreen :lightpink :lightsalmon :lightseagreen :lightskyblue :lightslategray :lightsteelblue :lightyellow :lime :limegreen :linen :magenta :maroon :mediumaquamarine :mediumblue :mediumorchid :mediumpurple :mediumseagreen :mediumslateblue :mediumspringgreen :mediumturquoise :mediumvioletred :midnightblue :mintcream :mistyrose :moccasin :navy :oldlace :olive :olivedrab :orange :orangered :orchid :palegoldenrod :palegreen :paleturquoise :palevioletred :papayawhip :peachpuff :peru :pink :plum :powderblue :purple :rebeccapurple :red :rosybrown :royalblue :saddlebrown :salmon :sandybrown :seagreen :seashell :sienna :silver :skyblue :slateblue :slategray  :springgreen :steelblue :tan :teal :thistle :tomato :turquoise :violet :wheat :yellow :yellowgreen)))

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
;; back ground color of the image
   (bg-color :accessor svg-bg-color
             :initarg :bg-color
             :initform "white")
   (objects :accessor svg-objects
            :initarg :objects
            :initform nil)
;; tags to label a few objects by color. (("string" . color))
   (tags :accessor svg-tags
         :initarg :tags
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
             (format nil "<path fill=\"node\" stroke=\"~A\" d=\"M~A,~AL~A,~A\" />"
                     (svg-object-color a)
                     (car l) (cdr l)
                     (car r) (cdr r))))
    (loop :for i :in (path-points a)
       :with previous = (car (path-points a))
       :collect (draw-segment previous i)
       :do (setq previous i))))

(defclass svg-circle (svg-object)
  ((center :accessor center
           :initarg :center)
   (radius :accessor radius
           :initarg :radius)))

(defmethod draw ((a svg-circle))
  (format nil
          "<circle fill=\"~A\" stroke=\"~A\" radius=\"~A\" center=\"~A,~A\" />"
          (svg-object-fill a)
          (svg-object-color a)
          (radius a)
          (car (center a))
          (cdr (center a))))

;; TODO: complete the svg-render function.
(defmethod draw ((a svg-image))
  (labels ((render-tag (a num)
             (format nil
                     "<text text-anchor=\"right\" stroke=\"~A\" x=\"870\" y=\"~A\">~A</text>~%"
                     (car (cdr a))
                     (+ 15 (* num 15))
                     (car a)))
           (svg-render (i)
             (let ((height (car (car (svg-image-header i))))
                   (width (cdr (car (svg-image-header i))))
                   (tags (svg-tags i)))
               (list (format nil "<svg height=\"~A\" width=\"~A\" >" height width)
                     (format nil "<polygon fill=\"~A\" stroke=\"none\" points=\"~A,~A ~A,~A ~A,~A ~A,~A\" />"
                             (svg-bg-color a)
                             0 0
                             height 0
                             height width
                             0 width)
                     (loop :for i :in tags :with c = 0
                        :collect (render-tag i c)
                        :do (incf c))
                     (mapcar #'draw (svg-objects i))
                     "</svg>"))))
    (list  (svg-xml-header a)
           (svg-doc-type a)
           (svg-render a))))

(defun write-svg-to-file (filename svg-drawn-image)
  (with-open-file (svg-stream filename
                              :direction :output
                              :if-exists :supersede)
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
