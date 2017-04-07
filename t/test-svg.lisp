(in-package :cl-user)
(defpackage :test-svg
  (:use :cl
        :prove
        :svg
        :svg.text
        :svg.rect
        :svg.object
        :svg.path
        :svg.polygon
        :svg.circle
        :svg.ellipse))

(in-package :test-svg)

(defun pentagon-points (center radius angle)
  "draw a regular pentagon at CENTER which can be circumscribed in RADIUS"
  (flet ((translate (to-point point-list) (mapcar (lambda (p) (cons (+ (car to-point) (car p))
                                                                    (+ (cdr to-point) (cdr p))))
                                                   point-list))
         (rotate (theta point-list) (mapcar (lambda (p) (cons (- (* (car p) (cos theta)) (* (cdr p) (sin theta)))
                                                              (+ (* (car p) (sin theta)) (* (cdr p) (cos theta)))))
                                            point-list))
         (regular-hexagon (r) (list (cons (* r (cos (/ pi 10))) (* r (sin (/ pi 10))))
                                    (cons 0 r)
                                    (cons (-(* r (cos (/ pi 10)))) (* r (sin (/ pi 10))))
                                    (cons (-(* r (cos (* 3 (/ pi 10))))) (-(* r (sin (* 3 (/ pi 10))))))
                                    (cons (* r (cos (* 3 (/ pi 10)))) (-(* r (sin (* 3 (/ pi 10)))))))))
    (translate center (rotate angle (regular-hexagon radius)))))

(defparameter *test-draw-path-cases*
  '(((:points ((1 . 1) (2 . 2) (3 . 3)) :stroke "red" :fill "green" :type :straight-line)
     . "<path stroke=\"red\" fill=\"green\" d=\"M 1 1 L 2 2 L 3 3\" />")
    ((:points ((2.34 . 8.6) (3.32 . 9.1) (8.4 . 10.11)) :fill "blue")
     . "<path stroke=\"black\" fill=\"blue\" d=\"M 2.34 8.6 L 3.32 9.1 L 8.4 10.11\" />"))
  "few paths to check svg-path")

(defun test-draw-svg-path (p)
  (flet ((points-string (a) (format nil "~A ~A" (car a) (cdr a))))
    (let ((fill-color (getf p :fill  "white"))
          (stroke-color (getf p :stroke "black"))
          (type (getf p :type  :straight-line))
          (points (getf p :points)))
      (cond ((eql type :straight-line)
             (draw (make-instance 'path 
                                  :points points 
                                  :fill-color fill-color 
                                  :stroke-color stroke-color)))))))
(defparameter *test-draw-polygon-cases*
  '(((:points ((1 . 1) (1 . 2) (2 . 2) (2 . 1)) :stroke "red" :fill "green")
     . "<polygon stroke=\"red\" fill=\"green\" points=\"1,1 1,2 2,2 2,1\" />"))
  "svg-polygon test cases")

(defun test-draw-svg-polygon (p)
  (flet ((points-string (a) (format nil "~A,~A" (car a) (cdr a))))
    (let ((fill-color (getf p :fill "white"))
          (stroke-color (getf p :stroke "black"))
          (points (getf p :points)))
      (draw (make-instance 'polygon
                           :points points
                           :fill-color fill-color
                           :stroke-color stroke-color)))))
(defparameter *test-draw-ellipse-cases* 
  '(((:center (10 . 10) :rx 4 :ry 3 :stroke "red" :fill "green")
     . "<ellipse stroke=\"red\" fill=\"green\" cx=\"10\" cy=\"10\" rx=\"4\" ry=\"3\" />"))
  "svg-circle test cases")

(defun test-draw-svg-ellipse (p)
  (let ((fill-color (getf p :fill "white"))
        (stroke-color (getf p :stroke "black"))
        (center (getf p :center))
        (rx (getf p :rx))
        (ry (getf p :ry)))
    (draw (make-instance 'ellipse
                         :center center
                         :rx rx
                         :ry ry
                         :stroke-color stroke-color
                         :fill-color fill-color))))
(defparameter *test-draw-circle-cases*
  '(((:center (10 . 10) :radius 10 :stroke "blue" :fill "cyan")
     . "<circle stroke=\"blue\" fill=\"cyan\" cx=\"10\" cy=\"10\" r=\"10\" />")))

(defun test-draw-svg-circle (p)
  (let ((fill-color (getf p :fill "white"))
        (stroke-color (getf p :stroke "black"))
        (center (getf p :center))
        (radius (getf p :radius)))
    (draw (make-instance 'circle
                         :center center
                         :radius radius
                         :stroke-color stroke-color
                         :fill-color fill-color))))

(defparameter *test-draw-rect-cases*
  '(((:small-point (10 . 11) :width 10 :height 20 :fill "violet" :stroke "red")
     . "<rect stroke=\"red\" fill=\"violet\" x=\"10\" y=\"11\" width=\"10\" height=\"20\" />")
    ((:small-point (80 . 10) :width 20 :height 10)
     . "<rect stroke=\"black\" fill=\"white\" x=\"80\" y=\"10\" width=\"20\" height=\"10\" />")))

(defun test-draw-svg-rect (p)
  (let ((fill-color (getf p :fill "white"))
        (stroke-color (getf p :stroke "black"))
        (pos (getf p :small-point))
        (width (getf p :width))
        (height (getf p :height)))
    (draw (make-instance 'rect
                         :small-point pos
                         :width width
                         :height height
                         :stroke-color stroke-color
                         :fill-color fill-color))))

(defparameter *test-draw-text-cases*
  '(((:content "content-check1" :text-pos (12 . 13) :fill-color "gold")
     . "<text fill=\"gold\" x=\"12\" y=\"13\" > content-check1 </text>")))
(defun test-draw-svg-text (p)
  (let ((fill-color (getf p :fill-color))
        (text-pos (getf p :text-pos))
        (content (getf p :content)))
    (draw (make-instance 'text
                         :text-pos text-pos
                         :content content
                         :fill-color fill-color))))

(defun test-draw-svg-image (img)
  (write-svg-to-file "test-svg-image-out.svg" img))

(defparameter *test-draw-image-cases*
  (list (make-instance 'svg-image
                       :image-size (cons 100 100)
                       :bg-color "silver"
                       :objects (list (make-instance 'text
                                                     :text-pos '(40 . 5)
                                                     :content "testing svg library"
                                                     :fill-color "magneta")
                                      (make-instance 'path
                                                        :points '((0 . 100) (100 . 0)))
                                      (make-instance 'ellipse
                                                     :center '(25 . 25)
                                                     :rx 4 :ry 3
                                                     :fill-color "red"
                                                     :stroke-color "blue")
                                      (make-instance 'circle
                                                     :center '(10 . 90)
                                                     :radius 7
                                                     :fill-color "blue"
                                                     :stroke-color "green")
                                      (make-instance 'rect
                                                     :small-point '(20 . 30)
                                                     :width 10 :height 5
                                                     :fill-color "black"
                                                     :stroke-color "darkred")
                                      (make-instance 'polygon
                                                     :points (pentagon-points '(50 . 50) 8 0)
                                                     :stroke-color "red")
                                      (make-instance 'polygon
                                                     :points (pentagon-points '(40 . 60) 5 (/ (* 2 pi) 3))
                                                     :stroke-color "cyan")
                                      (make-instance 'polygon
                                                     :points (pentagon-points '(60 . 40) 3 (/ pi 3))
                                                     :stroke-color "lightgreen")
                                      (make-instance 'polygon
                                                     :points (pentagon-points '(30 . 70) 4 pi)
                                                     :stroke-color "steelblue")))))

(plan 5)
(subtest "svg-path"
  (mapcar (lambda (c) (is (test-draw-svg-path (car  c)) (cdr c)))
          *test-draw-path-cases*))
(subtest "svg-polygon"
  (mapcar (lambda (c) (is (test-draw-svg-polygon (car c)) (cdr c)))
          *test-draw-polygon-cases*))
(subtest "svg-circle"
  (mapcar (lambda (c) (is (test-draw-svg-circle (car c)) (cdr c)))
          *test-draw-circle-cases*))
(subtest "svg-ellipse"
  (mapcar (lambda (c) (is (test-draw-svg-ellipse (car c)) (cdr c)))
          *test-draw-ellipse-cases*))
(subtest "svg-rect"
  (mapcar (lambda (c) (is (test-draw-svg-rect (car c)) (cdr c)))
          *test-draw-rect-cases*))
(subtest "svg-text"
  (mapcar (lambda (c) (is (test-draw-svg-text (car c)) (cdr c)))
          *test-draw-text-cases*))

(subtest "drawing a sample image using all elements: check manually"
  ; just one for now.
  (is (test-draw-svg-image (car *test-draw-image-cases*)) nil))
(finalize)
