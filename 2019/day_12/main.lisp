(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")

(defmacro zip3 (a b c)
  `(mapcar #'list ,a ,b ,c))

(defmacro zip (a b)
  `(mapcar #'list ,a ,b))

(defun deep-copy (lst)
  (cond
    ((null lst)
     '())
    (t
      (cons 
        (list (copy-list (caar lst)) (copy-list (cadar lst)))
        (deep-copy (cdr lst))))))

(defun remove-seq (l s)
  (cond
    ((null l)
     s)
    (t
      (remove-seq (cdr l) (remove (car l) s)))))

(defun parse-planet (line)
  (->> line
       (remove-seq (list #\x #\y #\z #\< #\> #\, #\=))
       (split-str " ")
       (mapcar #'parse-integer)))

(defun get-input (filename)
  (->> filename
       (read-lines)
       (mapcar 'parse-planet)
       (mapcar (lambda (pos) (list (list 0 0 0) pos)))))

(defun calc-grav (other pos vel)
  (mapcar
    (lambda (x)
      (cond
        ((> (second x) (third x))
         (setf (first x) (1- (first x))))
        ((< (second x) (third x))
         (setf (first x) (1+ (first x)))))
      (car x))
    (zip3 vel pos other)))

(defun gravity (lst)
  (labels ((helper (lst pos)
                   (cond
                     ((null lst)
                      (list 0 0 0))
                     (t
                       (calc-grav (second (car lst)) pos (helper (cdr lst) pos))))))
    (loop for pos in (mapcar 'cadr lst) collect (helper lst pos))))

;     vel       pos       d_vel
; (((0 0 0) (-9 -1 -1)) (3 3 -1))
(defun step-sim (lst)
  (mapcar
    (lambda (x)  
      (setf (caar x) (mapcar #'+ (caar x) (cadr x)))
      (setf (cadar x) (mapcar #'+ (caar x) (cadar x)))
      (car x))
    (zip lst (gravity lst))))

(defun energy (moon f)
  (reduce 
    (lambda (acc x) (+ acc (abs x)))
    (funcall f moon)
    :initial-value 0))

(defun total-energy (lst)
  (reduce 
    (lambda (acc x) (+ acc (* (energy x 'car) (energy x 'cadr))))
    lst
    :initial-value 0))

(defun part-one (input)
  (labels ((helper (lst n)
                   (cond
                     ((= n 0)
                      (total-energy lst))
                     (t 
                       (helper (step-sim lst) (1- n))))))

    (helper input 1000)))

(defun exit-cond (lst init f)
  (cond
    ((null lst)
     t)
    (t
      (and (= (funcall f (cadar lst)) (car init))
           (= (funcall f (caar lst)) 0)
           (exit-cond (cdr lst) (cdr init) f)))))

;     vel       pos       
; ((0 0 0) (-9 -1 -1))
(defun calc-axis (lst f)
  (let ((init (mapcar (lambda (x) (funcall f (cadr x))) lst)))
    (labels ((helper (lst i)
                     (cond 
                       ((exit-cond lst init f)
                        i)
                       (t
                         (helper (step-sim lst) (1+ i))))))

      (helper (step-sim lst) 1))))

(defun part-two (input)
  (lcm
    (calc-axis (deep-copy input) 'first)
    (lcm (calc-axis (deep-copy input) 'second)
         (calc-axis (deep-copy input) 'third))))

(defun main()
  (let ((input (get-input (input))))
    (my-timer 'Silver #'part-one (deep-copy input))
    (my-timer 'Gold #'part-two input)))
