;;;; cl-latin-hc.lisp

(in-package #:cl-latin-hc)

(defparameter a-coeff #(-3.969683028665376d+01
			2.209460984245205d+02
			-2.759285104469687d+02
			1.383577518672690d+02
			-3.066479806614716d+01
			2.506628277459239d+00))

(defparameter b-coeff #(-5.447609879822406d+01
			1.615858368580409d+02
			-1.556989798598866d+02
			6.680131188771972d+01
			-1.328068155288572d+01))

(defparameter c-coeff #(-7.784894002430293d-03
			-3.223964580411365d-01
			-2.400758277161838d+00
			-2.549732539343734d+00
			4.374664141464968d+00
			2.938163982698783d+00))

(defparameter d-coeff #(7.784695709041462d-03
			3.224671290700398d-01
			2.445134137142996d+00
			3.754408661907416d+00))

(defun polynomial-eval (x terms)
  (reduce (lambda (a b)
	    (+ (* a x) b))
	  terms :initial-value 0.0d0))
  
(defun low-condition (x)
  (let ((q (sqrt (* -2.0d0 (log x)))))
    (/ (polynomial-eval q c-coeff) (+ 1.0d0 (* q (polynomial-eval q d-coeff))))))
    
(defun middle-condition (x)
  (let* ((q (- x 0.5d0))
	 (r (* q q)))
    (/ (* q (polynomial-eval r a-coeff)) (+ 1.0d0 (* r (polynomial-eval r b-coeff))))))

(defun high-condition (x)
  (let ((q (sqrt (* -2.0d0 (log (- 1.0d0 x))))))
    (* -1.0d0 (/ (polynomial-eval q c-coeff) (+ 1.0d0 (* q (polynomial-eval q d-coeff)))))))

(defun normal-inverse-cdf (x)
  ;; Acklam's Algorithm
  (let* ((x-low 0.02425d0)
	 (x-high (- 1.0d0 x-low)))
    (cond
      ((= x 0.0d0) (* -1.0d0 most-positive-double-float))
      ((and (> x 0.0d0) (< x x-low)) (low-condition x))
      ((and (>= x x-low) (<= x x-high)) (middle-condition x))
      ((and (>= x x-high) (< x 1.0d0)) (high-condition x))
      ((= x 1.0d0) most-positive-double-float))))

(defun cdf-sampler (intervals)
  (lambda (m) (+ (* (/ 1.0d0 intervals)
		    (random 1.0d0))
		 (/ (- m 1.d0) intervals))))

(defun sample-latin-hyper-cube (samples dimensions)
  (declare (optimize (speed 3)(safety 0)(space 0)(debug 0)))
  ;;Set up the array
  (let ((lh-array (make-array (list samples dimensions)
			      :initial-element 0.0d0
			      :element-type 'double-float))
	(draw-samples (cdf-sampler samples)))
    (dotimes (j dimensions)
      ;; Since we have multiple dimensions we have to shuffle the elements
      (let ((samples-for-j
	      (alexandria:shuffle
	       (map 'vector #'normal-inverse-cdf
		    (map 'vector draw-samples
			 (alexandria:iota samples :start 1))))))
	(dotimes (i samples)
	  (setf (aref lh-array i j) (aref samples-for-j i)))))
    lh-array))


(defclass latin-hyper-cube ()
  ((samples
    :initarg :samples
    :accessor number-of-samples
    :initform (error "Specify number of samples"))
   (dimensions
    :initarg :dimensions
    :accessor number-of-dimensions
    :initform (error "Specify number of dimensions"))
   (sample-array
    :accessor sample-array)
   (current-sample
    :accessor current-sample
    :initform 0)))

(defmethod initialize-instance :after ((lhc latin-hyper-cube) &key)
  (setf (sample-array lhc)
	(sample-latin-hyper-cube (number-of-samples lhc)
				 (number-of-dimensions lhc))))

(defgeneric return-sample (a-sampler))

(defmethod return-sample ((a-sampler latin-hyper-cube))
  ;; If there are any samples left return a single sample
  (if (< (current-sample a-sampler) (number-of-samples a-sampler))
      (let ((a-sample
	      (select:select (sample-array a-sampler)
			     (current-sample a-sampler)
			     (select:range 0 (number-of-dimensions a-sampler)))))
	(incf (current-sample a-sampler))
	a-sample)
      (error "No samples left in hypercube!")))
	   
(defun lhs-closure (samples dimensions)
  (let ((a (make-instance 'latin-hyper-cube
			  :samples samples
			  :dimensions dimensions)))
    (lambda () (return-sample a))))
