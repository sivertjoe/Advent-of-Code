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

(defun move (pos facing)
  (cond
    ((= facing 0)
     (list (car pos) (1+ (cadr pos))))
    ((= facing 2)
     (list (car pos) (1- (cadr pos))))
    ((= facing 1)
     (list (1+ (car pos)) (cadr pos)))
    ((= facing 3)
     (list (1- (car pos)) (cadr pos)))))

(defun new-dir (facing out)
  (mod (+ facing (if (= out 1) 1 -1)) 4))

(defun count-panels (ins in f)
  (let ((comp (make-instance 'intcode-computer :instructions ins))
        (map (make-hash-table :test 'equal)))

    (labels ((helper (map comp in facing pos) 
        (setf (in comp) (list in))
        (let ((r1 (run comp)))
          (if (= (car r1) *exit*)
            (funcall f map)
            (let ((r2 (run comp)))
              (setf (gethash pos map) (third r1))
              (helper 
                map 
                comp 
                (gethash (move pos (new-dir facing (third r2))) map 0)
                (new-dir facing (third r2))
                (move pos (new-dir facing (third r2)))))))))

      (helper map comp in 0 (list 0 0)))))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defvar *white* #\#)
(defvar *black* #\SPACE)


(defun hash-key-values (map)
  (loop for k being each hash-key of map
     using (hash-value v)
     collect (list k v)))

(defun build-string (grid w)
  (labels ((helper (grid n)
    (cond
      ((= n 0)
        (format nil "~%~a" (coerce (make-array (array-total-size grid) :displaced-to grid) 'string)))
      (t
        (setf (aref grid (1- n) (1- w)) #\NewLine)
        (helper grid (1- n))))))
    (helper grid (array-dimension grid 0))))

(defun grid-color (c)
  (if (= c 1)
    *white*
    *black*))

(defun set-point (grid val)
  (setf 
    (aref grid (abs (cadr (car val))) (caar val)) 
    (grid-color (second val))))

(defun build-code (grid l w)
  (cond
    ((null l)
     (build-string grid w))
    (t
      (set-point grid (car l))
      (build-code grid (cdr l) w))))

(defun display-code (map)
  (let* ((x (reduce 'min (hash-keys map) :key 'car))
        (y (reduce 'min (hash-keys map) :key 'cadr))

        ; Width and height get +1 since the numbers are zero indexed
        ; E.g x = 5 => array width must be 6
        ; w gets an exsta +1 to accomodate newlines
        (w (+ 2 (- (reduce 'max (hash-keys map) :key 'car) x)))
        (h (1+ (- (reduce 'max (hash-keys map) :key 'cadr) y)))
        (grid (make-array (list h w) :initial-element #\SPACE)))
    (build-code grid (hash-key-values map) w)))
    
(defun part-one (input)
  (count-panels (get-array input) 0 'hash-table-count))

(defun part-two (input)
  (count-panels (get-array input) 1 'display-code))

(defun main()
  (let ((input (get-input (input))))
      (my-timer 'Silver #'part-one input)
      (my-timer 'Gold #'part-two input)))
