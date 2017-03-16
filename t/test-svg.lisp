(defpackage :test-svg
  (:use :cl
        :svg))

(in-package :test-svg)


(let ((simple-image (make-instance 'svg:svg-image :image-header '((1000 . 1000)))))
  (push (make-instance 'svg:svg-path
                       :color "blue"
                       :points (loop :for i :upto 1000 :collect (cons i (+ 250 (random 500)))))
        (svg:svg-objects simple-image))
  (svg:write-svg-to-file "./test-image.svg"
                            (svg:draw simple-image)))

