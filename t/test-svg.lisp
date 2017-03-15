(defpackage :test-my-svg
  (:use :cl
        :my-svg))

(in-package :test-my-svg)


(let ((simple-image (make-instance 'my-svg:svg-image :image-header '((1000 . 1000)))))
  (push (make-instance 'my-svg:svg-path 
                       :points (loop :for i :upto 500 :collect (cons i (random (1+ i)))))
        (my-svg:svg-objects simple-image))
  (my-svg:write-svg-to-file "./test-image.svg"
                            (my-svg:draw simple-image)))

