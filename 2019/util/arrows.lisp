(defun simple-inserter (insert-fun)
  (lambda (acc next)
    (if (listp next)
        (funcall insert-fun acc next)
        (list next acc))))

(defmacro -> (initial-form &rest forms)
  "Inserts INITIAL-FORM as first argument into the first of FORMS, the result
into the next, etc., before evaluation.  FORMS are treated as list designators."
  (reduce (simple-inserter #'insert-first)
          forms
          :initial-value initial-form))

(defmacro ->> (initial-form &rest forms)
  "Like ->, but the forms are inserted as last argument instead of first."
  (reduce (simple-inserter #'insert-last)
          forms
          :initial-value initial-form))

(defun insert-first (arg surround)
  "Inserts ARG into the list form SURROUND as its first argument, after the
operator."
  (list* (car surround)
         arg
         (cdr surround)))

(defun insert-last (arg surround)
  "Inserts ARG into the list form SURROUND as its last argument."
  (append surround (list arg)))
