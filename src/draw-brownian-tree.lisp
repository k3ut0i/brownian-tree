(in-package :brownian-tree)

;;; Utilities to draw images

;; Mostly for debugging small examples.
(defun pprint-buffer (stream buffer
		      &optional colon amp (delimiter #\Space))
  "Dump BUFFER data."
  (declare (type (simple-array * *) buffer)
	   (ignore colon amp)) ;; What am I actually ignoring?
  (let* ((dimensions (array-dimensions buffer))
	 (width (car dimensions))
	 (height (cadr dimensions)))
    (dotimes (i width)
      (dotimes (j height)
	(let* ((point (aref buffer i j))
	       (point-char (cond ((getf point :seed) #\S)
				 ((getf point :particle) (min 9 (getf point :depth)))
				 (t #\_))))
	  (princ point-char stream))
	(write-char delimiter stream))
      (write-char #\Linefeed stream))))

					;FIXME: Complete this method
(defun draw-tree (tree file-name output-type)
  "Draw the TREE to the output file FILE-NAME of type OUTPUT-TYPE."
  (declare (type brownian-tree tree)
	   (type (simple-array character *) file-name))
  (with-open-file (outf file-name
			:direction :output
			:if-exists :supersede)
    (case output-type
      (:lisp-data (format outf "~A" (slot-value tree 'buffer)))
      (:raw-data (format outf "~/brownian-tree:pprint-buffer/"
			 (slot-value tree 'buffer)))
      (:ppm-image (draw-ppm tree file-name))
      (:pbm-image (draw-pbm tree file-name))
      (:pgm-image (draw-pgm tree file-name))
      (:netpbm-image (draw-netpbm tree file-name))
      (:svg-image (draw-svg-from-tree tree file-name)))))


(defun draw-netpbm (tree file-name)
  "Draw the image in netpbm format from the TREE to FILE-NAME."
  (declare (type brownian-tree tree)
	   (type (simple-array character *) file-name))
  (with-open-file (outf file-name
			:direction :output
			:if-exists :supersede)
    (let ((image-type "P2")
	  (width (slot-value tree 'width))
	  (height (slot-value tree 'height))
	  (image-depth 255)
	  (particles (getf (slot-value tree 'attributes) :particle-count))
	  (seeds (getf (slot-value tree 'attributes) :seed-count)))
      (format outf "~A~%~A ~A~%~A~%" image-type width height image-depth)
      (dotimes (i width)
	(dotimes (j height)
	  (let* ((sval (aref (slot-value tree 'buffer) i j))
		 (color-val (cond ((null sval) 255)
				  ((getf sval :seed) 0)
				  ((getf sval :particle) 0))))
	    (format outf "~A " color-val)))
	(princ #\Linefeed outf)))))

(defun draw-pbm (tree file-name)
  "Draw the image in portable bitmap format from the TREE to FILE-NAME."
  (declare (type brownian-tree tree)
	   (type (simple-array character *) file-name))
  (with-open-file (outf file-name
			:direction :output
			:if-exists :supersede)
    (let ((image-type "P1")
	  (width (slot-value tree 'width))
	  (height (slot-value tree 'height)))
      (format outf "~%~A ~A~%" image-type width height)
      (dotimes (i width)
	(dotimes (j height)
	  (let* ((sval (aref (slot-value tree 'buffer) i j))
		 (color-val (cond ((null sval) 1)
				  ((getf sval :seed) 0)
				  ((getf sval :particle) 0))))
	    (format outf "~A " color-val)))
	(princ #\Linefeed outf)))))
(defun draw-ppm (tree file-name)
  '())

(defun draw-netpbm (tree file-name netpbm-type)
  (declare (type brownian-tree tree)
	   (type (simple-array character *) file-name)
	   (type keyword netpbm-type))
  (let* ((width (slot-value tree 'width))
	 (height (slot-value tree 'height))
	 (buffer (slot-value tree 'buffer))
	 (header (case netpbm-type
		   (:pbm (format nil "P1~%~A ~A~%" width height))
		   (:pgm (format nil "P2~%~A ~A~%255~%" width height))
		   (:ppm (format nil "P3~%~A ~A~%255~%" width height)))))
    (labels ((particle-ratio (s)
	       (/ (getf s :depth) (getf (slot-value tree 'attributes)
					:max-depth)))
	     (pgm-scale (s) (format nil "~S"
				    (+ 127 (floor (* 127 (particle-ratio s))))))
	     (ppm-scale (s)
	       (let ((ratio (particle-ratio s)))
		 (format nil "~A ~A ~A"
			 (floor (* 127 ratio))
			 (floor (+ 63 (* 180 ratio)))
			 (floor (- 255 (* 60 ratio))))))
	     (pixel-string (s)
	       (cond ((null s) (case netpbm-type
				 (:pbm "1")
				 (:pgm "255")
				 (:ppm "255 255 255")))
		     ((getf s :seed) (case netpbm-type
				       (:pbm "0")
				       (:pgm "0")
				       (:ppm "0 0 0")))
		     ((getf s :particle) (case netpbm-type
					   (:pbm "0")
					   (:pgm (pgm-scale s))
					   (:ppm (ppm-scale s)))))))
      (with-open-file (outf file-name :direction :output
			    :if-exists :supersede)
	(format outf "~A" header)
	(dotimes (i width)
	  (dotimes (j height)
	    (format outf "~A " (pixel-string (aref buffer i j))))
	  (princ #\Linefeed outf))))))

(Defun draw-svg (tree file-name)
  "Draw svg image from the TREE object to FILE-NAME."
  ;; (declare (type brownian-tree tree)
  ;; 	   (type ((simple-array character *) file-name)))
  (declare (ignore tree file-name))
  '())

