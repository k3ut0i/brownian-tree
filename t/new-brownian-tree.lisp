(defpackage :test-bt
  (:use :cl
        :brownian-tree
        :svg))
(in-package :test-bt)

(defun create-new-bt ()
  "create a new brownian tree"
  (let ((st (make-instance 'brownian-tree:brownian-tree)))
    (brownian-tree:update-point (cons 500 500) st)
    (brownian-tree:new-particle st)))
;; (let* ((rw (make-instance 'svg:svg-image :image-header (list (cons 1000 1000))))
;;        (walk1 nil) (walk2 nil) (walk3 nil) (walk4 nil) (walk5 nil) (walk6 nil) (walk7 nil)
;;        (debug t))
;;   (format debug "-------------black---------------~%")
;;   (setq walk1 (loop :thereis (brownian-tree:new-particle)))
;;   (push (make-instance 'svg:svg-path
;;                        :color "black"
;;                        :points walk1)
;;         (svg:svg-objects rw))
;;   (format debug "-------------red---------------~%")
;;   (setq walk2 (loop :thereis (brownian-tree:new-particle)))  
;;   (push (make-instance 'svg:svg-path
;;                        :color "red"
;;                        :points walk2)
;;         (svg:svg-objects rw))
;;   (format debug "-------------orange---------------~%")
;;   (setq walk3 (loop :thereis (brownian-tree:new-particle)))
;;   (push (make-instance 'svg:svg-path
;;                        :color "orange"
;;                        :points walk3)
;;         (svg:svg-objects rw))
;;   (format debug "-------------yellow---------------~%")
;;   (setq walk4 (loop :thereis (brownian-tree:new-particle)))  
;;   (push (make-instance 'svg:svg-path
;;                        :color "yellow"
;;                        :points walk4)
;;         (svg:svg-objects rw))
;;   (format debug "-------------green---------------~%")
;;   (setq walk5 (loop :thereis (brownian-tree:new-particle)))  
;;   (push (make-instance 'svg:svg-path
;;                        :color "green"
;;                        :points walk5)
;;         (svg:svg-objects rw))
;;   (format debug "-------------blue---------------~%")
;;   (setq walk6 (loop :thereis (brownian-tree:new-particle)))  
;;   (push (make-instance 'svg:svg-path
;;                        :color "blue"
;;                        :points walk6)
;;         (svg:svg-objects rw))
;;   (format debug "-------------violet---------------~%")
;;   (setq walk7 (loop :thereis (brownian-tree:new-particle)))  
;;   (push (make-instance 'svg:svg-path
;;                        :color "violet"
;;                        :points walk7)
;;         (svg:svg-objects rw))

;;   (svg:write-svg-to-file "./test-walk.svg"
;;                             (svg:draw rw)))
