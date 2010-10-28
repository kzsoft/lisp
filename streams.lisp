(setf path (make-pathname :name "myfile"))

(defun test1 ()
  (setf str (open path :direction :output
		:if-exists :supersede))
  (format str "Something~%")
  (close str))

(defun test2 ()
  (setf str (open path :direction :input))
  (read-line str)
  (close str))

(defun test3 ()
  (with-open-file (str path :direction :output
		     :if-exists :supersede)
    (format str "Something~%")))

