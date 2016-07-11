;; Quicksort is an efficient sorting algorithm, serving as a systematic
;; method for placing the elements of an array in order. Developed by Tony Hoare
;; in 1959, with his work published in 1961, it is still a commonly
;; used algorithm for sorting. [https://en.wikipedia.org/wiki/Quicksort]

;; Copyright (c) 2016 Volosincu Mihai-Bogdan



(defparameter *unsorted-list* '( 5 6 9 0 3 2 1 4 7 8))

(defun qs (piv lo hi)
  (let((pindex nil)
       (items (- hi lo))
       (nextpiv-b nil))

    ;; search the index of the pivot
    (labels ((search-pindex (a-list index)
	       (if(= (car a-list) piv)
		  index
		  (search-pindex (cdr a-list) (1+ index)))
	       ))
      (setq pindex (search-pindex *unsorted-list* 0)) )

    
    (print (list items " - "  pindex piv lo hi))

    ;; calculate a new pivot and call recursively
    (if(> items 1)
       (progn
	 (multiple-value-bind (int rest)
	     (floor (/ (+ pindex lo) 2))
	   (qs (nth int *unsorted-list*) lo pindex))
	 
	 (multiple-value-bind (int rest)
	     (floor (/ (+ pindex hi) 2))
	   (qs (nth int *unsorted-list*) pindex hi))))

    ))
