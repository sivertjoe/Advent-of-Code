(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")

(defvar *nums*
    (->> "input"
        (read-lines)
        (mapcar #'parse-integer)))

(defun two-adjacent (n)
  (labels ((helper (i s)
    (cond
      ((eql i 5) nil)
      ((eql (char s i) (char s (+ i 1))) t)
      (t (helper (+ i 1) s)))))
  (helper 0 (write-to-string n))))

(defun not-in-group (n)
  (labels ((helper (i s)
    (cond 
      ((= i 5) nil)
      ((char= (char s i) (char s (1+ i)))
       (cond
         ((= i 4) t)
         ((char= (char s (1+ i)) (char s (+ i 2)))
          (skip i s (char s i)))
         (t t)))
      (t (helper (1+ i) s))))

         (skip (i s c)
          (cond
            ((>= (+ i 3) 5) nil)
            ((char= (char s (+ i 3)) c) (skip (1+ i) s c))
            (t (helper (+ i 3) s)))))

    (helper 0 (write-to-string n))))

(defun increasing (n)
  (labels ((helper (i s)
    (cond
      ((eql i 5) t)
      ((char< (char s (+ i 1)) (char s i)) nil)
      (t (helper (+ i 1) s)))))
  (helper 0 (write-to-string n))))

(defun solve (test)
  (labels ((helper (curr end sum)
    (cond
      ((eql curr end) sum)
      ((funcall test curr) (helper (+ curr 1) end (+ sum 1)))
      (t (helper (+ curr 1) end sum)))))
    (helper (car *nums*) (cadr *nums*) 0)))

(defun part-one ()
  (solve (lambda (n) (and (two-adjacent n) (increasing n)))))

(defun part-two ()
  (solve (lambda (n) (and (not-in-group n) (increasing n)))))

(format t "~a~%" (part-one))
(format t "~a~%" (part-two))
