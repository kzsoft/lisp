;; (defstruct rectangle
;;  height width)

;; (defstruct circle
;;  radius)

;; (defun area (x)
;; (cond ((rectangle-p x)
;; 	 (* (rectangle-height x) (rectangle-width x)))
;; 	((circle-p x)
;; 	 (* pi (expt (circle-radius x) 2)))))

;; (defun test-area ()
;;  (let ((r (make-rectangle)))
;;   (setf (rectangle-height r) 2
;; 	  (rectangle-width r) 3)
;;    (area r)))

(defclass rectangle ()
  (height width))

(defclass circle ()
  (radius))

(defmethod area ((x rectangle))
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod area ((x circle))
  (* pi (expt (slot-value x 'radius) 2)))

(defun test-area ()
  (let ((r (make-instance 'rectangle)))
    (setf (slot-value r 'height) 2
	  (slot-value r 'width) 3)
    (area r)))

(defun test-area2 ()
  (let ((c (make-instance 'circle)))
    (setf (slot-value c 'radius) 3)
    (area c)))
