(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")

(defun get-input (filename)
  (->> filename
    (read-lines)
    (car)
    (split-str ",")
    (mapcar 'parse-integer)))
  
(defun get-num (array index)
  (aref array (aref array index)))

(defun handle (array index op)
  (let* ((fst (get-num array (+ index 1)))
        (snd (get-num array (+ index 2)))
        (res (funcall op fst snd)))
    (setf (aref array (aref array (+ index 3))) res))
    (run array (+ index 4)))

(defun run (array index)
  (let ((curr (aref array index)))
    (cond
      ((= curr 99)
       (aref array 0))
      ((= curr 1)
       (handle array index '+))
      ((= curr 2)
       (handle array index '*)))))

(defun get-array (input)
    (make-array (length input) :initial-contents input))

(defun compute-nouns (array n1 n2)
  (setf (aref array 1) n1)
  (setf (aref array 2) n2)
  (run array 0))

(defun part-one (input)
  (compute-nouns (get-array input) 12 2))

(defun part-two (input)
  (let ((array (get-array input)))
    (loop for a from 1 below 100 do
      (loop for b from 1 below 100
        if (= (compute-nouns (copy-seq array) a b) 19690720) 
          do (return-from part-two (+ (* 100 a) b))))))

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
