(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")
(load "../util/queue.lisp")

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
   (ht
     :initform (make-hash-table :test 'equal)
     :accessor ht)
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

(defparameter *wall* 0)
(defparameter *open* 1)
(defparameter *oxygen* 2)

(defun get-prev (prev cur)
  (cond
    ((< (car prev) (car cur)) ; prev.x < cur.x, e.g (1 0) (2 0), go west
     3)
    ((> (car prev) (car cur))
     4)
    ((< (cadr prev) (cadr cur)) ; prev.y < cur.y, e.g (0 1) (0 2), go south
     2)
    ((> (cadr prev) (cadr cur))
     1)
    (t
      (format t "UNREACHABLE")
      (exit))))
        
(defun neighbors (point)
  (list
    (list (1+ (car point)) (cadr point))
    (list (1- (car point)) (cadr point))
    (list (car point) (1+ (cadr point)))
    (list (car point) (1- (cadr point)))))

(defun move-bot (comp dir)
  (setf (in comp) (list dir))
  (third (run comp)))

(defun calculate-maze (comp at seen)
  (loop for n in (remove-if (lambda (x) (gethash x seen))(neighbors at))
    do (let ((status (move-bot comp (get-prev n at))))
         (setf (gethash n (ht comp)) status)
         (setf (gethash n seen) t)
         (unless (= status *wall*)
            (calculate-maze comp n seen)
            (move-bot comp (get-prev at n))))))

(defun shortest-paths (map from)
  (let ((visited (make-hash-table :test 'equal))
        (dist (make-hash-table :test 'equal))
        (q (make-queue :initial-contents (list (list from 0)))))
    (setf (gethash (list 0 0) visited ) t)

    (loop
      for nd = (queue-pop q)
      while nd
      finally (return dist)
      do (progn
           (setf (gethash (car nd) dist) (cadr nd))
           (loop for nbr in
                 (remove-if (lambda (x) (gethash x visited))(neighbors (car nd)))
                 do (progn
                      (setf (gethash nbr visited) t)
                      (if (not (= (gethash nbr map) *wall*))
                        (queue-push (list nbr (1+ (cadr nd))) q))))))))

(defun key-val (ht)
  (loop for k being the hash-keys in ht using (hash-value v)
      collect (list k v)))

(defun find-key (ht target)
  (labels ((helper (lst target)
                   (cond
                     ((= target (cadr (car lst)))
                      (car (car lst)))
                     (t
                       (helper (cdr lst) target)))))
    (helper (key-val ht) target)))

(defun shortest-path (from to maze)
  (let ((seen (make-hash-table :test 'equal)))
    (setf (gethash from seen) t)
    (let ((dist (shortest-paths maze from)))
      (gethash to dist))))

(defun get-maze (input)
  (let ((comp (make-instance 'intcode-computer :instructions (get-array input)))
        (seen (make-hash-table :test 'equal)))
    (setf (gethash (list 0 0) seen) t)
    (calculate-maze comp (list 0 0) seen)
    (ht comp)))

(defun part-one (input)
    (let* ((maze (get-maze input))
          (oxygen (find-key maze *oxygen*)))
      (shortest-path (list 0 0) oxygen maze)))

(defun part-two (input)
  (let* ((maze (get-maze input))
        (oxygen (find-key maze *oxygen*))
        (max-path 0))
    (reduce 
      (lambda (acc x) (max acc (cadr x))) 
      (key-val (shortest-paths maze oxygen))
      :initial-value 0)))

(defun main()
  (let ((input (get-input (input))))
      (my-timer 'Silver #'part-one input)
      (my-timer 'Gold #'part-two input)))
