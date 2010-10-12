(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str test p2)
		    nil)))
        nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\  ))))


(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
	  (parse-month (second toks))
	  (parse-integer (third toks)))))

(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun"
    "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str month-names
		     :test #'string-equal)))
    (if p
	(+ p 1)
        nil)))

(defun read-integer (str)
  (if (every #'digit-char-p str)
      (let ((accum 0))
	(dotimes (pos (length str))
	  (setf accum (+ (* accum 10)
			 (digit-char-p (char str pos)))))
	accum)
      nil))
