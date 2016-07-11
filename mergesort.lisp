;; Mergesort is an efficient, general-purpose, comparison-based
;; sorting algorithm. Mergesort is a divide and conquer algorithm
;; that was invented by John von Neumann in 1945. [https://en.wikipedia.org/wiki/Merge_sort]

;; Copyright (c) 2016 Volosincu Mihai-Bogdan



(defun mergesort (lo hi)
  (let ((mid 0)
	(items 0))

    ;; initializations
    (setq items (- hi lo))
    (multiple-value-bind (intreg rest)
	(floor (/ (+ hi (1+ lo)) 2))
      (setq mid intreg))

    ;; recursive call if more than 1 item
    (cond ((> items 1)
	   (mergesort lo mid)
	   (mergesort mid hi)))

    ;; merge step
    (let ((temp (sort-range *unsorted-list* lo mid hi)))
      (do ((x lo (1+ x))
	   (tx 0 (1+ tx)))
	  ((= x hi))
	(setf (nth x *unsorted-list*) (nth tx temp))))
    ))




;; collect and sort range between low and high

(defun sort-range (LIST lo mid hi)
  (labels ((collect-range (i1 i2)
	     (if (and (< i1 mid) (< i2 hi))
		 (let ((lv (nth i1 LIST))
		       (rv (nth i2 LIST)))
		   
		   (if(and (< lv rv) (< i1 mid))
		      (cons lv (collect-range (1+ i1) i2))
		      (if(<= i2 hi)
			 (cons rv (collect-range i1 (1+ i2))))
		      ))
		 (if (< i1 mid)
		     (cons (nth i1 LIST) (collect-range (1+ i1) i2))
		     (if(< i2 hi)
			(cons (nth i2 LIST) (collect-range i1 (1+ i2)))))
		 )))
    (collect-range lo mid)))





(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")


(defparameter *unsorted-list* '( 9 5 1 4 2 19 3 12 8 14 11 16 20 15 18 6 17 7 13 10))

(print *unsorted-list*)
(mergesort 0 20)
(print *unsorted-list*)



(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;")


(defparameter *unsorted-list* '(8 2 3 4 0 1 5 6 9 7))

(print *unsorted-list*)
(mergesort 0 10)
(print *unsorted-list*)



(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")

(defparameter *unsorted-list* '(5 2 1 4 8 3 7 6 0 9))

(print *unsorted-list*)
(mergesort 0 10)
(print *unsorted-list*)

