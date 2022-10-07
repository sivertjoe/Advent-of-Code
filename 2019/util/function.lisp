(defun compose (&rest functions)
  (if (= (length functions) 1)
      (car functions)
      (lambda (&rest args)
        (funcall (first functions)
                 (apply (apply #'compose (rest functions))
                        args)))))
