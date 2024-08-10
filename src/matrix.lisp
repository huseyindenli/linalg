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
(defun matrix-at (m row col)
  (cond ((row-vector-p m) (aref (row-vector-data m) col))
	((col-vector-p m) (aref (col-vector-data m) row))
	((matrix-p m)     (aref (matrix-data m)
				(+ (* row (matrix-cols m))
				   col)))
	(t (error "hata"))))
;; usage:
;; (matrix-at rv1 1 0) => 1.0
;; (matrix-at rv1 1 1) => 2.0
;; (matrix-at rv1 1 2) => 3.0
;; -------------------------------------------------------------------------------------------
(defun (setf matrix-at) (value m row col)
  (setf (aref (matrix-data m)
	      (+ (* row (matrix-cols m))
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
;; (create-matrix 5 5 (make-array 25 :initial-element 0.0) :generator #'(lambda (x y) (random 10.0)))
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
(defun transpose (m)
  (create-matrix (matrix-cols m)
		 (matrix-rows m)
		 (matrix-data m)
		 :generator #'(lambda (i j) (matrix-at m j i))))
;; -------------------------------------------------------------------------------------------
(defun print-matrix (m)
  (loop for i from 0 below (matrix-rows m) do
    (loop for j from 0 below (matrix-cols m) do
      (format t "~7,2f  " (matrix-at m i j)))
    (terpri)))
;; -------------------------------------------------------------------------------------------
(defun matrix-mul (m1 m2)
  (if (and (floatp m1) (floatp m2))
      (* m1 m2) 
      (if (and (floatp m1) (matrix-p m2))
	  (let ((res (make-matrix (matrix-rows m2) (matrix-cols m2))))
	    (loop for i from 0 below (matrix-rows m2) do
	      (loop for j from 0 below (matrix-cols m2) do
		(setf (matrix-at res i j)
		      (* (matrix-at m2 i j) m1))))
	    res)
	  (if (and (matrix-p m1) (floatp m2))
	      (let ((res (make-matrix (matrix-rows m1) (matrix-cols m1))))
		(loop for i from 0 below (matrix-rows m1) do
		  (loop for j from 0 below (matrix-cols m1) do
		    (setf (matrix-at res i j)
			  (* (matrix-at m1 i j) m2))))
		res)
	      (if (and (matrix-p m1) (matrix-p m2))
		  (progn (assert (= (matrix-cols m1) (matrix-rows m2))
				 nil
				 "you can not multiply them!")
			 (let ((res (make-matrix (matrix-rows m1) (matrix-cols m2))))
			   (loop for i from 0 below (matrix-rows res) do
			     (loop for j from 0 below (matrix-cols res) do
			       (loop for k from 0 below (matrix-cols m1) do
				 (incf (matrix-at res i j)
				       (* (matrix-at m1 i k)
					  (matrix-at m2 k j))))))
			   res)))))))
;; -------------------------------------------------------------------------------------------
(defun operator* (&rest operands)
  (push 1.0 operands)
  (when (consp operands)
    (let ((acc (car operands)))
      (dolist (o operands acc)
	(setf acc (matrix-mul acc o)))
      acc)))
;; usage:
;; (operator* 3.0 4.0) => 12.0
;; (operator* 2.0 rv1) => #(MATRIX 1 3 #(2.0 4.0 6.0))
;; (operator* rv1 2.0) => #(MATRIX 1 3 #(2.0 4.0 6.0))
;; (operator* rv1 cv1) => #(MATRIX 1 1 #(146.0))
;; (operator* cv1 rv1 m33-1) => #(MATRIX 3 3 #(336.6 402.6 468.6 642.6 768.6 894.60004 948.6 1134.6001 1320.6001)) == (matrix-mul cv1 (matrix-mul rv1 m33-1))
;; (operator* rv1 cv1 m33-1) => "you can not multiply them"
;; (operator* m33-1 m33-2) => #(MATRIX 3 3 #(32.459995 38.76 45.059998 70.259995 85.56 100.86 108.060005 132.36 156.66))
;; (operator* m33-1 m33-2 cv1) => #(MATRIX 3 1 #(2567.88 5696.28 8824.68)) == (matrix-mul m33-1 (matrix-mul  m33-2 cv1))
;; (operator* m33-1 m33-2 rv1) => "you can not multiply them"
;; (operator* m33-1 m33-2 2.0 cv1) => #(MATRIX 3 1 #(5135.76 11392.56 17649.36)) == (matrix-mul m33-1 (matrix-mul m33-2 (matrix-mul 2.0 cv1)))
;; -------------------------------------------------------------------------------------------
(defun matrix-add (&rest operands)
  (let* ((i (matrix-rows (car operands)))
	 (j (matrix-cols (car operands)))
	 (m (make-matrix i j)))
    (push m operands)
    (when (consp operands)
      (let ((acc (copy-matrix (car operands))))
	(dolist (o operands acc)
	  (assert-same-dimensions acc o)
	  (map-into (matrix-data acc) #'+ (matrix-data acc) (matrix-data o)))))))
;; usage:
;; (matrix-add m33-1 m33-2)
;; (matrix-add m33-1 m33-2 m33-2)

;; ? coklu argumanlarda yapilmak isteneni anlamadim.
;; (defun matrix-sub (&rest operands)
;;   (let* ((i (matrix-rows (car operands)))
;; 	 (j (matrix-cols (car operands)))
;; 	 (m (make-matrix i j)))
;;     (push m operands)
;;     (when (consp operands)
;;       (let ((acc (copy-matrix (car operands))))
;; 	;; (setf acc (matrix-mul -1.0 acc))
;; 	(dolist (o operands acc)
;; 	  (assert-same-dimensions acc o)
;; 	  (map-into (matrix-data acc) #'- (matrix-data acc) (matrix-data o)))))))
;; -------------------------------------------------------------------------------------------
(defun vec-module (v)
  (assert (or (row-vector-p v)
	     (col-vector-p v))
	  nil
	  "v must be an row or column vector")
  (sqrt (reduce #'(lambda (i j) (+ i (* j j))) (matrix-data v) :initial-value 0.0)))
;; usage:
;; (vec-module rv1)
;; (vec-module cv1)
;; -------------------------------------------------------------------------------------------
(defun normalized (v)
  (let ((s (vec-module v)))
    (if (/= s 0)
	(if (row-vector-p v)
	    (create-row-vector (matrix-cols v)
			       (map '(simple-array single-float 1) #'(lambda (i) (/ i s)) (matrix-data v)))
	    (if (col-vector-p v)
		(create-col-vector (matrix-cols v)
				   (map '(simple-array single-float 1) #'(lambda (i) (/ i s)) (matrix-data v)))))
	(copy-matrix v))))
;; usage:
;; (normalized (create-row-vector 3 #(0.0 0.0 0.0))) => #(MATRIX 1 3 #(0.0 0.0 0.0) ROW-VECTOR)
;; (normalized (create-row-vector 3 #(1.0 2.0 3.0))) => #(MATRIX 1 3 #(0.26726124 0.5345225 0.8017837) ROW-VECTOR)
;; -------------------------------------------------------------------------------------------
(defun vec-x (v)
  (aref (matrix-data v) 0))

(defun vec-y (v)
  (aref (matrix-data v) 1))

(defun vec-z (v)
  (aref (matrix-data v) 2))

;; -------------------------------------------------------------------------------------------
(defun cros-prod (v1 v2)
  (assert (and (is-3d-vector v1)
	     (is-3d-vector v2))
	  nil
	  "you can`t")
  (create-matrix 3 1 (make-array 3 :element-type 'single-float
				   :initial-contents (vector (- (* (vec-y v1) (vec-z v2)) (* (vec-y v2) (vec-z v1)))
							     (- (* (vec-z v1) (vec-x v2)) (* (vec-z v2) (vec-x v1)))
							     (- (* (vec-x v1) (vec-y v2)) (* (vec-x v2) (vec-y v1)))))))
;; -------------------------------------------------------------------------------------------

