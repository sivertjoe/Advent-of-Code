(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")

(defun get-input (input)
    (->> input
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

(defun solve (nums test)
  (labels ((helper (curr end sum)
    (cond
      ((eql curr end) sum)
      ((funcall test curr) (helper (+ curr 1) end (+ sum 1)))
      (t (helper (+ curr 1) end sum)))))
    (helper (car nums) (cadr nums) 0)))

(defun part-one (input)
  (solve input (lambda (n) (and (two-adjacent n) (increasing n)))))

(defun part-two (input)
  (solve input (lambda (n) (and (not-in-group n) (increasing n)))))

(defun color (sym res)
  (cond 
    ((eq sym 'Silver)
      (format nil "~c[34m~a~c[0m" #\ESC res #\ESC))
    ((eq sym 'Gold)
      (format nil "~c[33;10m~a~c[0m" #\ESC res #\ESC))))

(defun my-timer (sym f arg)
  (let* ((t0 (get-internal-real-time))
        (res (funcall f arg))
        (t1 (get-internal-real-time))
        (elapsed (round (/ (- t1 t0) 1000))))
    (format t "(~dms)~a~a~%" elapsed #\tab (color sym res))))

(defun input () 
  (if (eql (length *posix-argv*) 2)
    (cadr *posix-argv*)
    (if (probe-file "input")
      "input"
      (progn
        (format t "Cant find input file..~%")
        (exit)))))

(defun main()
  (let ((input (get-input (input))))
      (my-timer 'Silver #'part-one input)
      (my-timer 'Gold #'part-two input)))
