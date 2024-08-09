(in-package #:linalg)

(defmacro do-matrix ((m i j &optional elt) &body body)
  `((loop for ,i from 0 below (matrix-rows ,m) do
    (loop for ,j from 0 below (matrix-cols ,m) do
	  ,@(if elt
		`((symbol-macrolet ((,elt (matrix-data ,m ,i ,j)))
		    ,@body))
		body)))))
