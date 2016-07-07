

;; Rotate an array of n elements to the right by i steps. 
;; Copyright (c) 2016 Volosincu Mihai-Bogdan


 ;;(setq array (make-array (length k) :initial-contents '( 9 5 1 4 2 19 3 12 8 14 11 16 20 15 18 6 17 7 13 10)))

 (defun mergesort (vals)
   (labels ((ms (lo hi)
	      (let ((items (- hi lo))
		    (llo lo)
		    (hhi hi))
	(multiple-value-bind (mid zec)
		    (floor (/ (+ hi lo) 2))
	  (if(> items 2)
	  (progn
	    (print (list lo hi items mid zec))
	    (ms lo mid)
	    (ms mid hi)
	    (do ((j llo (+ j 1)))
		((= j hi)) 
	       (print j))
	     (print "---"))
	 nil
	 )))))
    (ms 0 20)))











