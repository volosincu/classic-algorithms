;; Quicksort is an efficient sorting algorithm, serving as a systematic
;; method for placing the elements of an array in order. Developed by Tony Hoare
;; in 1959, with his work published in 1961, it is still a commonly
;; used algorithm for sorting. [https://en.wikipedia.org/wiki/Quicksort]

;; Copyright (c) 2016 Volosincu Mihai-Bogdan


(defun quicksort (lo hi)

  (let((items (- hi lo))
       (range-pos '())
       (range-vals '())
       (pivot nil))

    (if (> items 1)
	(progn
	      
	  (do ((n hi (1- n)))
	      ((< n lo))
	    (push n range-pos )
	    (push (nth n *unsorted-list*) range-vals ))
	  
	  ;; choose a pivot different from min and max of partition
	  (let ((min (apply #'min range-vals))
		(max (apply #'max range-vals)))

	    (if (> (length range-pos) 2)
		(dolist (n range-pos)
		  (if(and (< min (nth n *unsorted-list*)) (> max (nth n *unsorted-list*)) (null pivot))
		     (setq pivot n)))
		(setq pivot
		      (nth (random (length range-pos))
			   range-pos))))

	  ;; ; swap values
	  (labels ((sort-partition (pval l h acc)
		     
		     (if(<= acc hi)
			(let ((temp nil))
			  
			  (do ((k l (incf l)))
			      ((>= (nth k *unsorted-list*) pval) ))
			  
			  (do ((k h (decf h)))
			      ((<= (nth k *unsorted-list*) pval) ))
			  
			  (progn
			       (setq temp (nth h *unsorted-list*))
			       (setf (nth h *unsorted-list*) (nth l *unsorted-list*))
			       (setf (nth l *unsorted-list*) temp))
			  
			  (sort-partition pval l h (1+ acc))
			  ))
		     ))
	    (sort-partition (nth pivot *unsorted-list*) lo hi lo))

  	  ;; recursive calls
	  (quicksort lo (- pivot 1))
	  (quicksort pivot hi))

	)))





(defparameter *unsorted-list* '( 5 6 9 0 3 2 1 4 7 8))
(quicksort 0 9)

(print *unsorted-list*)
(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")


(defparameter *unsorted-list* '( 5 6 7 9 0 3 1 4 2 8))
(quicksort 0 9)

(print *unsorted-list*)
(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")


(defparameter *unsorted-list* '(0 5 6 3 2 9 4 7 8 1))
(quicksort 0 9)


(print *unsorted-list*)
(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")


(defparameter *unsorted-list* '(11 9 1 13 12 5 0 19 8 14 3 18 2 15 16 17 6 4 7 10 ))
(quicksort 0 19)


(print *unsorted-list*)
(print  ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
