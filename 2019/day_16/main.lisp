(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")
(load "../util/function.lisp")

(defun get-input (filename)
  (-<>> filename
        (read-lines)
        (car)
        (coerce <> 'list)
        (mapcar (compose 'parse-integer 'string))))

(defparameter *base-pattern* 
  (make-array 4 :initial-contents (list 0 1 0 -1)))
(defparameter *len* (length *base-pattern*))

(declaim (inline elem))
(defun elem (i n)
  (declare (type (unsigned-byte 32) i)
           (type (unsigned-byte 32) n)
           (optimize (speed 3)))
  (let ((index (mod (floor i n) *len*)))
    (aref *base-pattern* index)))

(declaim (inline flat))
(defun flat (n)
  (declare (type (signed-byte 32) n)
           (optimize (speed 3)))
  (mod (abs n) 10))

(declaim (inline calc-sum))
(defun calc-sum (lst i pos)
  (declare (type (unsigned-byte 32) i)
           (type (unsigned-byte 32) pos)
           (optimize (speed 3)))
  (cond
    ((null lst)
     0)
    (t
      (+ 
        (* (car lst) (elem (1+ i) (1+ pos))) 
        (calc-sum (cdr lst) (1+ i) pos)))))
     
(declaim (inline calc-single))
(defun calc-single (lst i)
  (flat (calc-sum lst 0 i)))

(declaim (inline calc))
(defun calc (lst len)
  (declare (type (unsigned-byte 32) len)
           (optimize (speed 3)))
  (let ((new '()))
    (loop for i from (1- len) downto 0 do
      (setf new (cons (calc-single lst i) new)))
    new))

(defun take (lst n)
  (subseq lst 0 n))

(defun take2 (arr n)
  (let ((lst '()))
    (loop for i from 0 below n do
          (setf lst (cons (aref arr i) lst)))
    (reverse lst)))

(defun solve (input n)
  (let* ((lst (copy-list input))
        (len (length lst)))
    (dotimes (i n)
      (setf lst (calc lst len)))
    (list-to-string (take lst 8))))

(defun part-one (input)
  (solve input 100))

(defun list-to-string (lst)
    (format nil "~{~A~}" lst))

(defun get-offset (input)
  (parse-integer (list-to-string (take input 7))))

(defun streched-input (input offset)
  (let* ((lst '())
         (len (length input))
         (v (make-array len :initial-contents input))
         (array-len (- (* 10000 len) offset))
        (array (make-array array-len)))
        
    (loop for it from offset below (* 10000 len)
          for idx from 0
          do 
      (setf (aref array idx) (aref v (mod it len))))
    array))

(defun part-two (input)
  (let* ((offset (get-offset input))
        (streched (streched-input input offset))
        (len-streched (length streched)))

    (dotimes (i 100)
      (let ((sum 0))
        (loop for idx from (1- len-streched) downto 0 do
              (progn
                (setf (aref streched idx)
                      (flat (+ (aref streched idx) sum)))
                (setf sum (aref streched idx))))))
    (-> streched
        (take2 8)
        (list-to-string))))
  
(defun main()
  (let ((input (get-input (input))))
    (my-timer 'Silver #'part-one input)
    (my-timer 'Gold #'part-two input)))
