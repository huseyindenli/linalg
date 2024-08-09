(in-package #:linalg)
;; (ql:quickload 'linalg) (in-package #:linalg)
;; -------------------------------------------------------------------------------------------
(defstruct (matrix
	    (:conc-name matrix-)
	    (:constructor make-matrix (rows cols &optional data))
	    :named
	    (:predicate matrix-p)
	    (:type vector))
  (rows 'rows-number-mandatory    :type integer) 
  (cols 'cols-number-mandatory    :type integer)
  (data (make-array (* rows cols)
		    :initial-element 0.0
		    :element-type 'single-float)
   :type (simple-array single-float (*))))
;; -------------------------------------------------------------------------------------------
(defun matrix-at (a row col)
  (cond ((row-vector-p a) (aref (row-vector-data a) col))
	((col-vector-p a) (aref (col-vector-data a) row))
	((matrix-p a)     (aref (matrix-data a)
				(+ (* row (matrix-cols a))
				   col)))
	(t (error "hata"))))
;; usage:
;; (matrix-at rv1 1 0) => 1.0
;; (matrix-at rv1 1 1) => 2.0
;; (matrix-at rv1 1 2) => 3.0
;; -------------------------------------------------------------------------------------------
(defun (setf matrix-at) (value a row col)
  (setf (aref (matrix-data a)
	      (+ (* row (matrix-cols a))
		 col))
	value))
;; usage:
;; (setf (matrix-at m33-1 0 0) 99.0) => #(MATRIX 3 3 #(99.0 2.1 3.1 4.1 5.1 6.1 7.1 8.1 9.1))
;; -------------------------------------------------------------------------------------------
(defun create-matrix (row-number col-number data &key generator)
  (let ((m (make-matrix row-number col-number (make-array (* row-number col-number)
							  :initial-contents data
							  :element-type 'single-float))))
    (if (functionp generator)
	(progn
	  (setf (matrix-data m)
		(make-array (* row-number col-number)
			    :initial-contents data
			    :element-type 'single-float))
	  (dotimes (i (matrix-rows m) m)
	    (dotimes (j (matrix-cols m))
	      (setf (matrix-at m i j)
		    (funcall generator i j)))))
	(make-matrix (matrix-rows m) (matrix-cols m)))
    m))
;; usage:
;; (create-matrix 2 3 #(1.1 2.2 3.3 4.4 5.5 6.6))
;; (create-matrix 2 2 #(1.1 2.1 3.1 4.1) :generator #'(lambda (i j) (random 10.0))) => #(MATRIX 2 2 #(5.5699635 3.7676394 0.93762994 9.857626))
;; (create-matrix 5 5 (make-array 25 :initial-element 0.0))
;; -------------------------------------------------------------------------------------------
(defstruct (row-vector
	    (:include matrix (rows 1))
	    (:conc-name row-vector-)
	    (:constructor make-row-vector (rows cols &optional data))
	    :named
	    (:predicate row-vector-p)
	    (:type vector)))
;; -------------------------------------------------------------------------------------------
(defun create-row-vector (col-number data)
  (make-row-vector 1 col-number (make-array (* 1 col-number)
					    :initial-contents data
					    :element-type 'single-float)))
;; usage:
;; (create-row-vector 3 #(1.1 2.2 3.3))
;; -------------------------------------------------------------------------------------------
(defstruct (col-vector
	    (:include matrix (cols 1))
	    (:conc-name col-vector-)
	    (:constructor make-col-vector (rows cols &optional data))
	    :named
	    (:predicate col-vector-p)
	    (:type vector)))
;; -------------------------------------------------------------------------------------------
(defun create-col-vector (row-number data)
  (make-col-vector row-number 1 (make-array (* 1 row-number)
					    :initial-contents data
					    :element-type 'single-float)))
;; usage:
;; (create-col-vector 3 #(1.1 2.2 3.3))
;; -------------------------------------------------------------------------------------------
(defun transpose (a)
  (create-matrix (matrix-cols a)
		 (matrix-rows a)
		 (matrix-data a)
		 :generator #'(lambda (i j) (matrix-at a j i))))
;; -------------------------------------------------------------------------------------------
(defun print-matrix (a)
  (loop for i from 0 below (matrix-rows a) do
    (loop for j from 0 below (matrix-cols a) do
      (format t "~7,2f  " (matrix-at a i j)))
    (terpri)))
;; -------------------------------------------------------------------------------------------
(defun matrix-mul (a b)
  (if (and (floatp a) (floatp b))
      (* a b) 
      (if (and (floatp a) (matrix-p b))
	  (let ((res (make-matrix (matrix-rows b) (matrix-cols b))))
	    (loop for i from 0 below (matrix-rows b) do
	      (loop for j from 0 below (matrix-cols b) do
		(setf (matrix-at res i j)
		      (* (matrix-at b i j) a))))
	    res)
	  (if (and (matrix-p a) (floatp b))
	      (let ((res (make-matrix (matrix-rows a) (matrix-cols a))))
		(loop for i from 0 below (matrix-rows a) do
		  (loop for j from 0 below (matrix-cols a) do
		    (setf (matrix-at res i j)
			  (* (matrix-at a i j) b))))
		res)
	      (if (and (matrix-p a) (matrix-p b))
		  (progn (assert (= (matrix-cols a) (matrix-rows b))
				 nil
				 "you can not multiply them!")
			 (let ((res (make-matrix (matrix-rows a) (matrix-cols b))))
			   (loop for i from 0 below (matrix-rows res) do
			     (loop for j from 0 below (matrix-cols res) do
			       (loop for k from 0 below (matrix-cols a) do
				 (incf (matrix-at res i j)
				       (* (matrix-at a i k)
					  (matrix-at b k j))))))
			   res)))))))

;; ERROR: does not work properly.
(defun operator* (&rest operands)
  (when (consp operands)
    (let ((acc (car operands)))
      (dolist (o operands acc)
	(setf acc (matrix-mul acc o))))))

