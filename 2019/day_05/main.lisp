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

(defun get-param (array index v)
  (let ((mode (digit (aref array index) (+ v 1))))
    (if (eql mode 0)
      (aref array (aref array (+ index v)))
      (aref array (+ index v)))))

(defun handle (array index input op &optional flag)
  (let* ((fst (get-param array index 1))
        (snd (get-param array index 2))
        (res (funcall op fst snd)))
    (if flag
        (setf (aref array (aref array (+ index 3))) (if res 1 0))
        (setf (aref array (aref array (+ index 3))) res))
    (run array (+ index 4) input)))

(defun digit (a b)
  (mod (floor a (expt 10 b)) 10))

(defun op-code (array index)
  (+ (* 10 (digit (aref array index) 1)) (digit (aref array index) 0)))

(defun run (array index input)
  (let ((op (op-code array index)))
    (cond
      ((= op 99)
       (aref array 0))
      ((= op 1)
       (handle array index input '+))
      ((= op 2)
       (handle array index input '*))
      ((= op 3)
       (setf (aref array (aref array (+ index 1))) input)
       (run array (+ index 2) input))
      ((= op 4)
       (let ((v (get-param array index 1)))
         (if (not (eql v 0))
           v
           (run array (+ index 2) input))))
      ((= op 5)
       (if (not (eql (get-param array index 1) 0))
         (run array (get-param array index 2) input)
         (run array (+ index 3) input)))
      ((= op 6)
       (if (eql (get-param array index 1) 0)
         (run array (get-param array index 2) input)
         (run array (+ index 3) input)))
      ((= op 7)
       (handle array index input '< t))
      ((= op 8)
       (handle array index input '= t)))))

(defun get-array (input)
    (make-array (length input) :initial-contents input))

(defun part-one (input)
  (run (get-array input) 0 1))

(defun part-two (input)
  (run (get-array input) 0 5))

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
