

;; Rotate an array of n elements to the right by i steps. 
;; Copyright (c) 2016 Volosincu Mihai-Bogdan



(defun create-array (n)
  (let ((values '()))
    (dotimes (j n) (push (- n j) values))
    ;;todo (make-list n)
    (make-array n
       :adjustable t 
       :initial-contents values)))



(defun rotate (r array)
  (let ((i 0)
	(temp nil)
	(copya (make-array 0 :adjustable t))) ;; make copy of array 
    (dotimes (j (length array))
      (cond ((and (>= j r))
	       ;; interchange elements using a temp variable 
	       (progn
	         (setq temp (aref copya i))
	         (setf (aref copya i) (aref copya j))
	         (setf (aref copya j) temp)
	         (incf i)))))
   copya))
  


(setq unar (create-array 10))
(print (rotate 5 unar))
(print (copy-seq unar)) ;; old array remains unmodified
