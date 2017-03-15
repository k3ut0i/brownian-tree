(defpackage :test-bt
  (:use :cl
        :brownian-tree
        :my-svg))
(in-package :test-bt)

(let* ((rw (make-instance 'my-svg:svg-image :image-header (list (cons 1000 1000))))
       (walk1 nil)
       (walk2 nil)
       (walk3 nil))
  (setq walk1 (loop :thereis (brownian-tree:new-particle)))
  (push (make-instance 'my-svg:svg-path
                       :color "black"
                       :points walk1)
        (my-svg:svg-objects rw))
  (setq walk2 (loop :thereis (brownian-tree:new-particle)))  
  (push (make-instance 'my-svg:svg-path
                       :color "red"
                       :points walk2)
        (my-svg:svg-objects rw))
  (setq walk3 (loop :thereis (brownian-tree:new-particle)))
  (push (make-instance 'my-svg:svg-path
                       :color "blue"
                       :points walk3)
        (my-svg:svg-objects rw))
  (my-svg:write-svg-to-file "./test-walk.svg"
                            (my-svg:draw rw)))
