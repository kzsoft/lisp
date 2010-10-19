;; author: king
;; date: Oct, 17 2010 10 18
;; note: The code document show that conditionals of lisp.
(defun test_when_odd (number)
  (when (oddp number)
    (format t "Hmm, ~A is odd.~%" number)
    (+ number 1)))

;; Now we write equivalent to up code.
(defun test_if_odd (number)
  (if (oddp number)
      (progn
	(format t "Hmm, ~A is odd.~%" number)
	(+ number 1))))

;; In lisp, the mother of all conditionals is cond, 
;; but the body will be evaluated only if the test expression return false.
;; cond brings two new advantages:
;; it allows multiple conditions,
;; and the code associated with each has an implicit progn.
(defun test_cond_odd (number)
  (cond ((oddp number) (format t "Hmm, ~A is odd.~%" number))
	(+ number 1)))

(defun test_conditionals ()
  (dolist (number '(1 2 3 4 5 6))
    (test_cond_odd number)))

;; Now, we test case conditional.
(defun month-length (mon)
  (case mon
    ((jan mar may jul aug oct dec) 31)
    ((apr jun sept nov) 30)
    (feb (if (leap-year) 29 28))
    (otherwise "unknown month")))

(defun get_current_time ()
   (multiple-value-bind (s i h d m y) (get-decoded-time)
     (format t "~A-~A-~A ~A:~A:~A~%" y m d h i s)))
