(defpackage :test-my-svg
  (:use :cl
        :my-svg))

(in-package :test-my-svg)


(let ((simple-image (make-instance 'my-svg:svg-image :image-header '((1000 . 1000)))))
  (push (make-instance 'my-svg:svg-path
                       :color "blue"
                       :points (loop :for i :upto 1000 :collect (cons i (+ 250 (random 500)))))
        (my-svg:svg-objects simple-image))
  (my-svg:write-svg-to-file "./test-image.svg"
                            (my-svg:draw simple-image)))

