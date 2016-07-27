(load "heap.lisp")


(defparameter *heapsort* nil)


(setq *heapsort* (make-array (length *readin*)
			     :initial-element nil
			     :fill-pointer 0))

(defun heapsort (heap)
  (let ((fp (fill-pointer *heapsort*)))
	(setf (fill-pointer *heapsort*) (+ 1 fp))
	(setf (aref *heapsort* fp) (aref heap 0))

	(if (> (length heap) 1)
	    (progn
	      ;; free heap array containe in build heap
	      (clean-heap (- (length heap) 1))
	      (heapsort (build-heap
			 (make-array (- (length heap) 1)
				     :displaced-to heap
				     :displaced-index-offset 1 ) 0))
	      )
	)
    ))






(clean-heap (length *readin*))
(heapsort (build-heap *readin* 0))
(print "Print sorted heap: ")
(print *heapsort*)

