;; Bubblesort is a simple sorting algorithm that repeatedly steps through
;; the list to be sorted, compares each pair of adjacent items and swaps
;; them if they are in the wrong order. [https://en.wikipedia.org/wiki/Bubble_sort]


;; Copyright (c) 2016 Volosincu Mihai-Bogdan




(defun bubblesort-iter ()

    (do ((k 0 (incf k)))
	((= k (length values)))

      (do ((j 0 (incf j)))
	    ((= j (length values)))
	  (if(> (nth k values) (nth j values))
	     (let ((temp nil))

	       (setq temp (nth j values))
	       (setf (nth j values) (nth k values))
	       (setf (nth k values) temp)
	     )))

    ))



(defun bubblesort-rec (lst k &optional j)
  (if (car lst)

	(if (null j)
	    (bubblesort-rec values k 0)
	    (if(car lst)
	       (progn
		 (if (> (nth k values) (nth j values))
		     (let ((temp nil))
		       (setq temp (nth j values))
		       (setf (nth j values) (nth k values))
		       (setf (nth k values) temp)
		       ))
	       (bubblesort-rec (cdr lst) k (incf j)))
	       ))
	    (if (and (not (null j))
		     (< k (- (length values) 1)))
		(bubblesort-rec (nthcdr k values) (incf k)))
      ))







(defparameter values '(4 8 7 0 1 2 3 9 5 6) )
(bubblesort-rec values 0)
(print values)



(defparameter values '(4 8 7 0 1 2 3 9 5 6) )
(bubblesort-iter)
(print values)
