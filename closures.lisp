(defun combiner (x)
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))

(defun combine (&rest args)
  (apply (combiner (car args))
	 args))
