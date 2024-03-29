(defun compose (&rest functions)
  (if (= (length functions) 1)
      (car functions)
      (lambda (&rest args)
        (funcall (first functions)
                 (apply (apply #'compose (rest functions))
                        args)))))
(defun bfs (&key from end neighbors test)
  (let ((visited (make-hash-table :test test))
        (queue (make-queue :initial-contents (list (list from 0)))))
    (setf (gethash from visited ) t)
    (loop for next = (queue-pop queue)
          while next
          when (equal (car next) end)
          return (cadr next)
          do
          (loop for neighbor in (funcall neighbors next)  do
                (unless (gethash neighbor visited)
                  (setf (gethash neighbor visited) t)
                  (queue-push (list neighbor (1+ (cadr next))) queue))))))
