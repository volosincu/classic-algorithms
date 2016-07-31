;; Insertion sort iterates, consuming one input element each repetition,
;; and growing a sorted output list. Each iteration, insertion sort removes
;; one element from the input data, finds the location it belongs within
;; the sorted list, and inserts it there. It repeats until no input elements remain.
;; [https://en.wikipedia.org/wiki/Insertion_sort]

;; Copyright (c) 2016 Volosincu Mihai-Bogdan





(defun insertion-sort (lst i &optional j)
  (if(car lst)
     (if (< j (length values))
	 (let ((el (nth i values))
	       (k (- j 1)))
	   (if (and (> k 0) (< el (nth k values)))
	       (let ((tmp (nth (+ k 1) values)))
		 (setf (nth (+ k 1) values) (nth k values)))
	       (setf (nth (+ 1 k) values) el)
	       )
	   (print (list (- 9 i) j))
	 (insertion-sort (cdr lst) i (incf j))))
     (if( < i (length values))
	(insertion-sort (nthcdr (incf i) values) i 0))
     ))









(defparameter values '(4 8 7 0 1 2 3 9 5 6) )
(insertion-sort values 0 0)
