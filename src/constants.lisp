(in-package #:linalg)

(defparameter rvi (create-row-vector 3 #(1.0 0.0 0.0)))
(defparameter rvj (create-row-vector 3 #(0.0 1.0 0.0)))
(defparameter rvk (create-row-vector 3 #(0.0 0.0 1.0)))

(defparameter cvi (create-col-vector 3 #(1.0 0.0 0.0)))
(defparameter cvj (create-col-vector 3 #(0.0 1.0 0.0)))
(defparameter cvk (create-col-vector 3 #(0.0 0.0 1.0)))


(defparameter rv1 (create-row-vector 3 #(1.0 2.0 3.0)))
(defparameter rv2 (create-row-vector 3 #(10.0 20.0 30.0)))

(defparameter cv1 (create-col-vector 3 #(11.0 21.0 31.0)))
(defparameter cv2 (create-col-vector 3 #(101.0 201.0 301.0)))

(defparameter m23 (create-matrix 2 3 #(1.1 2.2 3.3 4.4 5.5 6.6)))
(defparameter m32 (create-matrix 3 2 #(1.11 2.22 3.33 4.44 5.55 6.66)))

(defparameter m33-1 (create-matrix 3 3 #(1.1 2.1 3.1 4.1 5.1 6.1 7.1 8.1 9.1)))
(defparameter m33-2 (create-matrix 3 3 #(1.2 2.2 3.2 4.2 5.2 6.2 7.2 8.2 9.2)))
