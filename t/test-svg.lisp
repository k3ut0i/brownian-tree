(in-package :cl-user)
(defpackage :test-svg
  (:use :cl
        :prove
        :svg.object
        :svg.path
        :svg.polygon))

(in-package :test-svg)


(defparameter *test-draw-path-cases*
  '(((:points ((1 . 1) (2 . 2) (3 . 3)) :stroke "red" :fill "green" :type :straight-line)
     . "<path stroke=\"red\" fill=\"green\" d=\"M 1 1 L 2 2 L 3 3\" />")
    ((:points ((2.34 . 8.6) (3.32 . 9.1) (8.4 . 10.11)) :fill "blue")
     . "<path stroke=\"black\" fill=\"blue\" d=\"M 2.34 8.6 L 3.32 9.1 L 8.4 10.11\" />"))
  "few paths to check svg-path")

(defun test-draw-svg-path (p)
  "for a given list of points P check  draw value"
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
     . "<polygon stroke=\"red\" fill =\"green\" points=\"1,1 1,2 2,2 2,1\" />"))
  "svg-polygon test cases")

(defun test-draw-svg-polygon (p)
  "for a given list of points P construct a polygon object and check it."
  ())

(defun test-draw-svg-circle ()
  ())

(defun test-draw-svg-image ()
  ())

(defun test-svg-class ()
  ())

(plan 4)
;; TODO: instead of just AND use subtests to check each case.
;; In the process look at how test suites are constructed. 
(subtest "testing svg-path"
  (mapcar (lambda (c) (is (test-draw-svg-path (car  c)) (cdr c)))
          *test-draw-path-cases*))
(pass (test-draw-svg-image))
(pass (test-draw-svg-polygon nil))
(pass (test-draw-svg-circle))
(finalize)
