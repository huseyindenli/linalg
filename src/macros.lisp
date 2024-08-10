(in-package #:linalg)

(defmacro do-matrix ((m i j &optional elt) &body body)
  `((loop for ,i from 0 below (matrix-rows ,m) do
    (loop for ,j from 0 below (matrix-cols ,m) do
	  ,@(if elt
		`((symbol-macrolet ((,elt (matrix-data ,m ,i ,j)))
		    ,@body))
		body)))))

;; (defmacro def-elementwise-op-fun (name op)
;;   `(defun ,name (&rest operands)
;;      (when )))

(defmacro assert-same-dimensions (a b)
  `(assert (and (=  (matrix-rows ,a) (matrix-rows ,b))
	      (=  (matrix-cols ,a) (matrix-cols ,b)))
	   nil
	   "you can not add them."
	   ,a ,b))

(defun is-3d-row-vec-p (v)
  (and (= (matrix-cols v) 3)
     (row-vector-p v)))

(defun is-3d-col-vec-p (v)
  (and (= (matrix-rows v) 3)
     (col-vector-p v)))

(defun is-3d-vector (v)
  (or (is-3d-row-vec-p v)
     (is-3d-col-vec-p v)))
