(define-modify-macro my-incf (&optional (y 1)) +)

(define-modify-macro append-val (val)
  (lambda (lst val) (append lst (list val))))

(defun test-my-incf ()
  (let ((x 1))
    (my-incf x)
    x))

(defun test-append-val ()
  (let ((lst '(a b c)))
    (append-val lst 'd)
    lst))
