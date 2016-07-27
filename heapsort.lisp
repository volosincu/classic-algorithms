
;; Heapsort was invented by J. W. J. Williams in 1964.
;; Heapsort can be thought of as an improved selection sort: like that algorithm,
;; it divides its input into a sorted and an unsorted region, and it iteratively shrinks
;; the unsorted region by extracting the largest element and moving that to the
;; sorted region. [https://en.wikipedia.org/wiki/Heapsort]


;; Copyright (c) 2016 Volosincu Mihai-Bogdan

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

