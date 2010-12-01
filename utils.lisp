(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

(defun test-for ()
  (for x 1 8
    (princ x)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		     choices)))))

(defun test-in ()
  (in (car '(+ 1)) '+ '- '*))

(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
	    (mapcar #'(lambda (expr)
			`(,(incf key) ,expr))
		    exprs))))

(defun test-random-choice ()
  (random-choice 1 2 3 4 5 6 7 8 9 10))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defun test-avg ()
  (avg 1 2 3))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(defun test-with-gensyms ()
  (with-gensyms (x y z)
    nil))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun test-aif ()
  (aif (+ 1 1)
       (1+ it)
       0))
