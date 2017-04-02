(in-package :cl-user)
(defpackage :test-svg
  (:use :cl
        :prove
        :svg
        :svg.object
        :svg.path
        :svg.polygon
        :svg.circle
        :svg.ellipse))

(in-package :test-svg)


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

(defun test-draw-svg-image ()
  (write-svg-to-file "test-svg-image-out.svg"
                     (make-instance 'svg-image
                                    :image-size (cons 100 100)
                                    :bg-color "cyan"
                                    :objects (list (make-instance 'circle 
                                                                  :center (cons 10 10)
                                                                  :radius 3
                                                                  :fill-color "red"
                                                                  :stroke-color "blue")
                                                   (make-instance 'ellipse 
                                                                  :center (cons 20 20) 
                                                                  :rx 4 :ry 3
                                                                  :fill-color "blue"
                                                                  :stroke-color "red")))))

(plan 4)
(subtest "svg-path"
  (mapcar (lambda (c) (is (test-draw-svg-path (car  c)) (cdr c)))
          *test-draw-path-cases*))
(subtest "svg-polygon"
  (mapcar (lambda (c) (is (test-draw-svg-polygon (car c)) (cdr c)))
          *test-draw-polygon-cases*))
(subtest "svg-circle - skeleton"
  (mapcar (lambda (c) (is (test-draw-svg-circle (car c)) (cdr c)))
          *test-draw-circle-cases*))
(subtest "svg-ellipse - skeleton"
  (mapcar (lambda (c) (is (test-draw-svg-ellipse (car c)) (cdr c)))
          *test-draw-ellipse-cases*))
(subtest "drawing a sample image using all elements: check manually"
  (pass "just passing through" ))
(finalize)
