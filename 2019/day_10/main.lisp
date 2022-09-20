(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")

(defun get-input (filename)
  (let ((l (read-lines filename)))
    (make-array (list (length l) (length (car l))) :initial-contents l)))

(defmacro iter-grid (array x y &rest body)
  `(loop for ,y from 0 to (1- (car (array-dimensions ,array))) do
         (loop for ,x from 0 to (1- (cadr (array-dimensions ,array)))
               do (progn ,@body))))

(defun angle (x y cx cy)
  (let* ((dx (- x cx))
         (dy (- y cy))
         (g (gcd dx dy)))
    (list (floor dx g) (floor dy g))))

(defun count-visible (array cx cy)
  (let ((seen (make-hash-table :test 'equal)))
    (iter-grid array x y
               (if (and 
                     (not (and (= cx x) (= cy y))) 
                     (eql (aref array x y) #\#))
                 (setf (gethash (angle x y cx cy) seen) 1)))
    (hash-table-count seen)))

(defun find-best (array)
  (let ((best 0)
        (pos (list 0 0)))
    (iter-grid array x y
               (if (eql (aref array x y) #\#)
                 (progn
                   (let ((new (count-visible array x y)))
                     (if (> new best)
                       (progn
                         (setf pos (list x y))
                         (setf best new)))))))
    pos))

(defparameter treshhold 0.001)
(defun my-round (v &optional (n 3))
  (declare (type float v)
           (type (integer 0) n))
  (let ((10^-n (expt 10 (- n))))
    (* (fround v 10^-n)
       10^-n)))

(defun atan2 (a b)
  (atan (- (car a) (cadr b)) (- (cadr a) (car b))))

(defun my-angle (a b)
  (let ((angle (my-round (- (/ pi 2) (atan2 a b)))))
    (if (< angle 0)
      (+ (* 2 pi) angle)
      angle)))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun find-nth (ht vals n)
  (labels ((helper (ht vals i n)
                   (let ((curr (gethash (aref vals (mod i (length vals))) ht)))
                     (cond 
                       ((= i (1- n))
                        (let ((last (car curr)))
                          (+ (* (car last) 100) (cadr last))))
                       (t
                         (setf 
                           curr
                           (cdr curr))
                         (helper ht vals (1+ i) n))))))
    (helper ht vals 0 (1- n))))

(defun part-one (input)
  (let ((pos (find-best input)))
    (count-visible input (car pos) (cadr pos))))

(defun part-two (input)
  (let ((pos (find-best input))
        (ht (make-hash-table :test 'equal)))
    (iter-grid input x y
               (if (and 
                     (eql (aref input y x) #\#) 
                     (not (equal pos (list x y))))
                 (let ((angle (my-angle pos (list x y))))
                   (setf (gethash angle ht) (cons (list x y) (gethash angle ht '()))))))

    (let* ((lst (sort (hash-keys ht) #'>))
           (vals (make-array (length lst) :initial-contents lst)))
      (find-nth ht vals 200))))

(defun main()
  (let ((input (get-input (input))))
    (my-timer 'Silver #'part-one input)
    (my-timer 'Gold #'part-two input)))
