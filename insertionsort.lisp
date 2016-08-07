;; Insertion sort iterates, consuming one input element each repetition,
;; and growing a sorted output list. Each iteration, insertion sort removes
;; one element from the input data, finds the location it belongs within
;; the sorted list, and inserts it there. It repeats until no input elements remain.
;; [https://en.wikipedia.org/wiki/Insertion_sort]

;; Copyright (c) 2016 Volosincu Mihai-Bogdan


(defun insertion-sort (i &optional j elem ielem)
  
  (if (null j)
      ;;recursive branch to iterate over j 
      (insertion-sort i (+ i 1))

      ;; insertion step
      (if (and (>= j 0) (< i (- (length values) 1)))
	  (let((v elem)
	       (vi ielem))
	    
	    (if (null v)
		(progn
		  (setq vi (+ i 1))
		  (setq v (nth vi values))))
	    
	    (if (< v (nth j values))
		(let((tmp v))
		  
		  (setf (nth vi values) (nth j values))
		  (setf (nth j values) v)
		  (setq vi j)))
	    ;; decrement j and send further the value of
	    ;; current selection and new index after switch
	    (insertion-sort i (decf j) v vi))

	  ;; if insertion step ended for j increment i and reset j
	  ;; recursive branch to iterate over i
	  (if(< i (length values))
	     (progn
	       (insertion-sort (incf i))))
	  )))



(defparameter values '(4 0 7 8 6 2 3 9 5 1))
(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
(print values)
(insertion-sort 0)
(print values)



(defparameter values '(4 8 7 0 1 2 3 9 5 6))
(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
(print values)
(insertion-sort 0)
(print values)



(defparameter values '(1 6 2 3 8 7 0 9 5 4))
(print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
(print values)
(insertion-sort 0)
(print values)
