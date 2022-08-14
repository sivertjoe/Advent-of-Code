(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")

(defun letter (s)
    (char s 0))
(defun numberx (s)
    (parse-integer (subseq s 1)))

(defvar *nums*
    (->> "input"
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

(defun solve (dist)
  (defun update (l n ht)
    (let ((curr (list 0 0)) 
          (sum (list 0))) 
      (loop for x in l do (handle ht x curr sum (ar n))) 
      ht))

    (-<>> (make-hash-table :test #'equal)
      (update (car *nums*) 0)
      (update (cadr *nums*) 1)
      (key-values)
      (remove-if (lambda (l) (or (not (inter (second l))) (equal (list 0 0) l))))
      (reduce #'min <> :key (lambda (x) (funcall dist (first x) (second x))))))

(defun dist (k v)
   (+ (abs (first k)) (abs (second k))))
    
(defun part-one ()
  (solve #'dist))

(defun dist2 (k v)
  (+ (second (first v)) (second (second v))))

(defun part-two ()
  (solve #'dist2))

(format t "~a~%" (part-one))
(format t "~a~%" (part-two))
