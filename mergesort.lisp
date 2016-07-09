

;; Rotate an array of n elements to the right by i steps. 
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
    (let ((temp (sort-range *unsorted-array* lo mid hi)))
      (do ((x lo (1+ x))
	   (tx 0 (1+ tx)))
	  ((= x hi))
	(setf (aref *unsorted-array* x) (nth tx temp))))
    ))




;; collect and sort range between low and high

(defun sort-range (ARRAY lo mid hi)
  (labels ((collect-range (i1 i2)
	     (if (and (< i1 mid) (< i2 hi))
		 (let ((lv (aref ARRAY i1))
		       (rv (aref ARRAY i2)))

		   (if(and (< lv rv) (< i1 mid))
		      (cons lv (collect-range (1+ i1) i2))
		      (if(<= i2 hi)
			 (cons rv (collect-range i1 (1+ i2))))
		      ))
		 (if (< i1 mid)
		     (cons (aref ARRAY i1) (collect-range (1+ i1) i2))
		     (if(< i2 hi)
			(cons (aref ARRAY i2) (collect-range i1 (1+ i2)\
							     ))))
		 )))
    (collect-range lo mid)))





(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\
;;;;")

(defvar *a-list-a* '( 9 5 1 4 2 19 3 12 8 14 11 16 20 15 18 6 17 7 13 1\
		     0))
(setq *unsorted-array* (make-array (length *a-list-a*) :initial-content\
				   s *a-list-a*))

(print *unsorted-array*)
(mergesort 0 20)
(print *unsorted-array*)

(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\
;;;;")

(defvar *a-list-b* '(8 2 3 4 0 1 5 6 9 7))
(setq *unsorted-array* (make-array (length *a-list-b*) :initial-content\
				   s *a-list-b*))

(print *unsorted-array*)
(mergesort 0 10)
(print *unsorted-array*)

(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\
;;;;")

(defvar *a-list-c* '(5 2 1 4 8 3 7 6 0 9))
(setq *unsorted-array* (make-array (length *a-list-c*) :initial-content\
				   s *a-list-c*))

(print *unsorted-array*)
(mergesort 0 10)
(print *unsorted-array*)










