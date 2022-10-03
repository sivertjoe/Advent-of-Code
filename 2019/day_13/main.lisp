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
   (arc
     :initform nil
     :initarg :arc
     :accessor arc)
   (ball
     :initform nil
     :accessor ball)
   (paddle
     :initform nil
     :accessor paddle)
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

(defun run-with-set (comp val )
  (setf (index comp) val)
  (run comp))

(defun run (comp )
  (let ((op (op-code comp)))
    (let ((x (cond
      ((= op 99)
       (list 1 -1))
      ((= op 1)
       (handle comp '+))
      ((= op 2)
       (handle comp '*))
      ((= op 3)
       (setf (in comp) (list (get-run-input comp)))
       (mem-write comp (mem-write-addr comp *par1*) (consume-in comp))
       (run-with-inc comp 2))
      ((= op 4)
       (let ((out (get-param comp *par1*)))
         (inc-index comp 2)
         (list 0 comp out)))
      ((= op 5)
       (if (not (eql (get-param comp *par1*) 0))
         (run-with-set comp (get-param comp *par2*) )
         (run-with-inc comp 3)))
      ((= op 6)
       (if (eql (get-param comp *par1*) 0)
         (run-with-set comp (get-param comp *par2*) )
         (run-with-inc comp 3)))
      ((= op 7)
       (handle comp '< t))
      ((= op 8)
       (handle comp '= t))
      ((= op 9)
       (setf (rel-base comp) (+ (rel-base comp) (get-param comp *par1*)))
       (run-with-inc comp 2)))))
    x)))

(defun digit (a b)
  (mod (floor a (expt 10 b)) 10))

(defun get-array (input)
    (make-array (length input) :initial-contents input))

(defparameter *empty* 0)
(defparameter *wall* 1)
(defparameter *block* 2)
(defparameter *paddle* 3)
(defparameter *ball* 4)

(defun count-blocks (map)
  (let ((c 0))
  (loop for k being the hash-keys in map using (hash-value v)
      do (if (= v *block*)
           (incf c)))
  c))

(defun create-map (ins in)
  (let ((comp (make-instance 'intcode-computer :instructions ins))
        (map (make-hash-table :test 'equal)))

    (labels ((helper (map comp in)
        (setf (in comp) (list in))
        (let ((r1 (run comp )))
          (if (= (car r1) *exit*)
            map
            (let ((r2 (run comp ))
                  (r3 (run comp )))
              (setf
                (gethash (list (third r1) (third r2)) map)
                (third r3))
              (helper map comp in))))))

      (helper map comp in))))

(defun part-one (input)
  (count-blocks (create-map (get-array input) nil)))

(defun get-run-input (comp)
  (let ((ball (ball comp))
        (paddle (paddle comp)))
    (cond
      ((> (car ball) (car paddle)) 1)
      ((< (car ball) (car paddle)) -1)
      (t 0))))

(defun get-outputs (r1 r2 r3)
  (values (third r1) (third r2) (third r3)))

(defun beat-game (comp score)
    (let ((r1 (run comp)))
      (if (= (car r1) *exit*)
        (car score)
        (let* ((r2 (run comp))
               (r3 (run comp)))
          (multiple-value-bind (x y z) (get-outputs r1 r2 r3)
            (if (and (= x -1) (= y 0))
              (setf (car score) z)
              (progn
                (cond
                  ((= z *ball*)
                   (setf (ball comp) (list x y)))
                  ((= z *paddle*)
                   (setf (paddle comp) (list x y))))
                (setf (gethash (list x y) (arc comp)) z))))

          (beat-game comp score)))))

(defun part-two (input)
  (let ((map (create-map (get-array input) nil)))
    (setf (car input) 2)
    (let ((comp (make-instance 'intcode-computer :instructions (get-array input) :arc map))
        (score (list 0)))
      (beat-game comp score))))

(defun main()
  (let ((input (get-input (input))))
      (my-timer 'Silver #'part-one input)
      (my-timer 'Gold #'part-two input)))
