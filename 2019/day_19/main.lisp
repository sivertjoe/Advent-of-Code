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

(defvar *positional* 0)
(defvar *immediate* 1)
(defvar *relative* 2)

(defvar *par1* 1)
(defvar *par2* 2)
(defvar *par3* 3)

(defvar *halt* 0)
(defvar *exit* 1)

(defclass intcode-computer ()
  ((memory
     :initform (make-hash-table)
     :accessor memory)
   (index
     :initform 0
     :accessor index)
   (input
     :initform nil
     :initarg :in
     :accessor in)
   (output
     :initform '()
     :accessor output)
   (rel-base
     :initform 0
     :accessor rel-base)
   (instructons
     :initarg :instructions
     :accessor instructions)))

(defun consume-in (comp)
  (let ((val (car (in comp))))
    (setf (in comp) (cdr (in comp)))
    val))

(defun mem-write (comp addr val)
  (if (>= addr (length (instructions comp)))
    (setf (gethash addr (memory comp)) val)
    (setf (aref (instructions comp) addr) val)))

(defun mem-read (comp addr)
  (if (>= addr (length (instructions comp)))
    (gethash addr (memory comp) 0)
    (aref (instructions comp) addr)))

(defun op (comp)
  (aref (instructions comp) (index comp)))

(defun get-index (comp &key (offset 0) (rel-base 0))
  (+ rel-base (aref (instructions comp) (+ (index comp) offset))))

(defun mode-off (comp off)
  (digit (op comp) (+ 1 off)))

(defun mem-write-addr (comp off)
  (let ((mode (digit (op comp) (1+ off))))
    (cond
      ((= mode *positional*)
       (get-index comp :offset off))
      ((= mode *relative*)
       (get-index comp :rel-base (rel-base comp) :offset off)))))

(defun op-code (comp)
  (+ (* 10 (digit (op comp) 1)) (digit (op comp) 0)))

(defun get-param (comp off)
  (let ((mode (digit (op comp) (1+ off))))
    (cond
      ((= mode *positional*)
       (mem-read comp (get-index comp :offset off)))
      ((= mode *immediate*)
       (mem-read comp (+ (index comp) off)))
      ((= mode *relative*)
       (mem-read comp (get-index comp :rel-base (rel-base comp) :offset off))))))

(defun handle (comp op &optional flag)
  (let* ((fst (get-param comp *par1*))
         (snd (get-param comp *par2*))
         (res (funcall op fst snd))
         (res-addr (mem-write-addr comp *par3*)))
    (if flag
      (mem-write comp res-addr (if res 1 0))
      (mem-write comp res-addr res))

    (run-with-inc comp 4)))

(defun inc-index (comp inc)
  (setf (index comp) (+ (index comp) inc)))

(defun run-with-inc (comp inc)
  (inc-index comp inc)
  (run comp))

(defun run-with-set (comp val)
  (setf (index comp) val)
  (run comp))

(defun run (comp)
  (let ((op (op-code comp)))
    (cond
      ((= op 99)
       (list 1 -1))
      ((= op 1)
       (handle comp '+))
      ((= op 2)
       (handle comp '*))
      ((= op 3)
       (mem-write comp (mem-write-addr comp *par1*) (consume-in comp))
       (run-with-inc comp 2))
      ((= op 4)
       (let ((out (get-param comp *par1*)))
         (inc-index comp 2)
         (setf (output comp) (cons out (output comp)))
         (list 0 comp out)))
      ((= op 5)
       (if (not (eql (get-param comp *par1*) 0))
         (run-with-set comp (get-param comp *par2*))
         (run-with-inc comp 3)))
      ((= op 6)
       (if (eql (get-param comp *par1*) 0)
         (run-with-set comp (get-param comp *par2*))
         (run-with-inc comp 3)))
      ((= op 7)
       (handle comp '< t))
      ((= op 8)
       (handle comp '= t))
      ((= op 9)
       (setf (rel-base comp) (+ (rel-base comp) (get-param comp *par1*)))
       (run-with-inc comp 2)))))

(defun digit (a b)
  (mod (floor a (expt 10 b)) 10))

(defun get-array (input)
  (make-array (length input) :initial-contents input))

(defun key-val (ht)
  (loop for k being the hash-keys in ht using (hash-value v)
        collect (list k v)))

(defparameter *scaffolding* 35)
(defparameter *open-space* 46)
(defparameter *newline* 10)

(defun create-map (input)
  (flet ((computer (in) (make-instance 'intcode-computer 
                                       :instructions (get-array input)
                                       :in in)))
    (let  ((map (make-hash-table :test 'equal)))
      (dotimes (y 50)
        (dotimes (x 50)
          (let ((out (third (run (computer (list x y))))))
            (setf (gethash (list x y) map) out))))
      map)))


(defun part-one (input)
  (let ((map (create-map input)))
    (loop for kv in (key-val map)
          sum (cadr kv))))

(defun print-beam (map)
  (dotimes (y 50)
    (dotimes (x 50)
      (format t "~a" (if (= (gethash (list x y) map) 1) #\# #\.)))
    (format t "~%")))

(defun computer (input x y)
  (make-instance 'intcode-computer :instructions (get-array input) :in (list x y)))

(defun compute-s (input x y)
  (third (run (computer input x y))))

(defun part-two (input)
  (let ((x 0)
        (y 0))
    (loop 
      do 
      (progn
        (loop
          while (= (compute-s input x (+ y 99)) 0)
          do (progn 
               (incf x)))

        (when (= (compute-s input (+ x 99) y) 1)
          (return (+ (* 10000 x) y)))
        (incf y)))))

(defun main()
  (let ((input (get-input (input))))
    (my-timer 'Silver #'part-one input)
    (my-timer 'Gold #'part-two input)))
