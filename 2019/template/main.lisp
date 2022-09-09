(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")

(defun get-input (filename)
  (->> filename
    (read-lines)))

(defun part-one (input)
  0)

(defun part-two (input)
  0)

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
