
;; Heap is a partial ordening data structure of a set of values

;; Heap a heap is a specialized tree-based data structure that
;; satisfies the heap property: If A is a parent node of B then the key (the value)
;; of node A is ordered with respect to the key of node B with the
;; same ordering applying across the heap. [https://en.wikipedia.org/wiki/Heap_(data_structure)]


;; Copyright (c) 2016 Volosincu Mihai-Bogdan


(defparameter *heap* nil)
(defparameter *readin* '())
(defparameter *dynamic-input* nil)


(defmacro parent-index (i)
  `(multiple-value-bind (a b)
       (floor (/ (1- ,i) 2))
     a))

(defmacro parent-node (i)
  `(aref *heap* (parent-index ,i)))




(format t "Do you want to insert manually the heap length and values ? yes/no ")
(let ((answer (read)))
  (if(eq answer 'yes)
     (setq *dynamic-input* t)))



(let ((nodes-count 0)
      (default-answer '(7 8 5 6 1 3 0 2 4 9))
      (user-input '()))
  
  (if(eq *dynamic-input* t)
     (progn
     (format t "Insert how many nodes the heap will have: ")
     (setq nodes-count (read))
     (format t "~%Now please set the values of the ~D ~S ~%~%" nodes-count "nodes.")
     
     (do ()
	 ((= nodes-count (length *readin*)))
       (format t "Insert the ~D'th node ~% " (+ 1 (length *readin*)))
       (push (read) user-input))
     (setq *readin* (make-array (length user-input) :initial-contents user-input)))
     ;;else
     (setq *readin* (make-array (length default-answer) :initial-contents default-answer))))


(defun clean-heap (length)
  (setq *heap*
	(make-array length
		    :initial-element nil
		    :fill-pointer 0))
  )

(clean-heap (length *readin*))

(defun build-heap (lst acc)
  (if (> (length lst) 0)
      (let ((fp (fill-pointer *heap*))
	    (node (aref lst 0)))

	(setf (fill-pointer *heap*) (incf fp))
	(setf (aref *heap* acc) node)

	;; call recursively swap-nodes function in case child is bigger than parent
	(labels ((swap-nodes (i)
		   (if (> i 0)
		       (if (> node (parent-node i))
			   (let ((temp (parent-node i))
				 (pi (parent-index i)))
			     (setf (aref *heap* pi) node)
			     (setf (aref *heap* i) temp)
			     (swap-nodes pi))
			   ))
		   ))
	  (swap-nodes acc))

	(build-heap (make-array (- (length lst) 1)
			     :displaced-to lst
			     :displaced-index-offset 1) (1+ acc))
	*heap*)))




(build-heap *readin* 0)
(print "Print max heap: ")
(print *heap*)
(print "")

  
 





