(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")

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
       (list 1 -1))
      ((= op 1)
       (handle array index input '+))
      ((= op 2)
       (handle array index input '*))
      ((= op 3)
       (setf (aref array (aref array (+ index 1))) (car input))
       (run array (+ index 2) (cdr input)))
      ((= op 4)
       (let ((v (get-param array index 1)))
         (list 0 (+ index 2) v))) ; Halt execution
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

(defun create-input (setting n out)
  (list (nth n setting) out))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))

(defun calc-thrust (array setting init)
  (->> init
    (create-input setting 0)
    (run (copy-seq array) 0)
    (third)

    (create-input setting 1)
    (run (copy-seq array) 0)
    (third)

    (create-input setting 2)
    (run (copy-seq array) 0)
    (third)

    (create-input setting 3)
    (run (copy-seq array) 0)
    (third)

    (create-input setting 4)
    (run (copy-seq array) 0)
    (third)))

(defun gen (n1 n2)
    (loop for i from n1 below (1+ n2) collect i))

(defun part-one (input)
  (reduce 
    'max 
    (mapcar (lambda (x) (calc-thrust (get-array input) x 0)) (all-permutations (gen 0 4)))
    :initial-value 0))


(defun gen-settings (list i out)
  (if (< i 5) 
    (list (nth (mod i 5) list) out)
    (list out)))

(defun exec (amps list i in)
  (let* ((am (nth (mod i 5) amps))
         (r (run (car am) (cadr am) in)))
    (cond
      ((= (car r) 1)
       (car in))
      (t
        (setf (second (nth (mod i 5) amps)) (second r))
        (exec 
          amps 
          list 
          (1+ i) 
          (gen-settings list (1+ i) (third r)))))))

(defun check-seed (array seed)
  (let ((A (list (copy-seq array) 0)) ; Array - Instruction Counter - Output
         (B (list (copy-seq array) 0)) 
         (C (list (copy-seq array) 0)) 
         (D (list (copy-seq array) 0)) 
         (E (list (copy-seq array) 0)))
    (exec 
      (list A B C D E) 
      seed
      0
      (gen-settings seed 0 0))))

(defun part-two (input)
  (let* ((array (get-array input)))
    (reduce 
    'max 
    (mapcar (lambda (x) (check-seed array x)) (all-permutations (gen 5 9)))
    :initial-value 0)))

(defun main()
  (let ((input (get-input (input))))
      (my-timer 'Silver #'part-one input)
      (my-timer 'Gold #'part-two input)))
