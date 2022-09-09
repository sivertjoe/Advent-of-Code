(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")

(defun letter (s)
    (char s 0))
(defun numberx (s)
    (parse-integer (subseq s 1)))

(defun get-input (input)
    (->> input
        (read-lines)
        (mapcar #'(lambda (s) (split-str "," s))))) 

(defun handle (table x curr sum acs)
    (defun helper (table curr incx incy num)
        (dotimes (n num)
             (setf (car curr) (+ (car curr) incx))
             (setf (cadr curr) (+ (cadr curr) incy))
             (setf (car sum)  (+ (car sum) 1))

            (let* ((copy (copy-list curr))
                   (l (gethash copy table (list (list nil nil) (list nil nil))))
                   (new (funcall acs l (car sum))))
                (setf (gethash copy table) new))))
     
    (cond
        ((char= (letter x) #\R)
         (helper table curr 1 0 (numberx x)))
        
        ((char= (letter x) #\L)
         (helper table curr -1 0 (numberx x)))
        
        ((char= (letter x) #\U)
         (helper table curr 0 1 (numberx x)))
        
        ((char= (letter x) #\D)
         (helper table curr 0 -1 (numberx x)))))

(defun ar (n)
  (lambda (l sum)
    (unless (second (nth n l))
      (setf (second (nth n l)) sum))
    (setf (first (nth n l)) t)
    l))

(defun inter (l)
  (and (caar l) (caadr l)))

(defun key-values (ht)
    (loop for k being the hash-keys in ht using (hash-value v)
      collect (list k v)))

(defun solve (dist nums)
  (defun update (l n ht)
    (let ((curr (list 0 0)) 
          (sum (list 0))) 
      (loop for x in l do (handle ht x curr sum (ar n))) 
      ht))

    (-<>> (make-hash-table :test #'equal)
      (update (car nums) 0)
      (update (cadr nums) 1)
      (key-values)
      (remove-if (lambda (l) (or (not (inter (second l))) (equal (list 0 0) l))))
      (reduce #'min <> :key (lambda (x) (funcall dist (first x) (second x))))))

(defun dist (k v)
   (+ (abs (first k)) (abs (second k))))
    
(defun part-one (input)
  (solve #'dist input))

(defun dist2 (k v)
  (+ (second (first v)) (second (second v))))

(defun part-two (input)
  (solve #'dist2 input))

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
