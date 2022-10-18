(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")
(load "../util/function.lisp")
(load "../util/queue.lisp")

(defun get-input (filename)
  (->> filename
       (read-lines)))

(defun key-val (ht)
  (loop for k being the hash-keys in ht using (hash-value v)
        collect (list k v)))


(defconstant +deal-into-new-stack+ "deal into new stack")
(defconstant +deal-with-increment+ "deal with increment")
(defconstant +cut+ "cut")

(defun is-command (line str)
  (and
    (>= (length line) (length str))
    (string= (subseq line 0 (length str)) str)))

(defun get-num (line str)
  (parse-integer (subseq line (length str))))

(defun increment-num (s)
  (get-num s +deal-with-increment+))

(defun mod-pow (num power modulus)
  (declare (type (integer 0) power modulus)
           (type integer num)
           (values (integer 0)))
  (labels ((modpower-r (b e a)
             (declare (type (integer 0) b e a))
             (if (zerop e)
                 a
                 (modpower-r (mod (* b b) modulus)
                             (truncate e 2)
                             (if (oddp e)
                                 (mod (* b a) modulus)
                                 a)))))
    (modpower-r (mod num modulus) power 1)))


(defun take-back (array n)
  (let ((new (make-array (length array)))
        (index 0))

    (loop for i from (- (length array) n) below (length array) do
          (progn
            (setf (aref new index) (aref array i))
            (incf index)))

    (loop for i from 0 below (- (length array) n) do
          (progn
            (setf (aref new index) (aref array i))
            (incf index)))
    new))

(defun take-front (array n)
  (let ((new (make-array (length array)))
        (index 0))

    (loop for i from n below (length array) do
          (progn
            (setf (aref new index) (aref array i))
            (incf index)))

    (loop for i from 0 below n do
          (progn
            (setf (aref new index) (aref array i))
            (incf index)))
    new))

(defun cut (array n)
  (if (< n 0)
    (take-back array (abs n))
    (take-front array n)))

(defun deal (array)
  (reverse array))

(defun deal-inc (array n)
  (let ((new (make-array (length array)))
        (place 0))
    (loop for card across array
          do 
          (progn
            (setf (aref new place) card)
            (setf place (mod (+ place n) (length array)))))
    new))

(defun make-deck (size)
  (make-array size
              :initial-contents (loop for i from 0 below size collect i)))

(defun shuffle (input size)
  (let ((cards (make-deck size)))
    (loop for line in input do
          (progn
            (cond
              ((is-command line +deal-with-increment+)
               (let ((num (increment-num line)))
                     (setf cards (deal-inc cards num))))
              ((is-command line +deal-into-new-stack+)
               (setf cards (deal cards)))
               
              ((is-command line +cut+)
               (let ((num (get-num line +cut+)))
                 (setf cards (cut cards num)))))))
    cards))

(defun get-number (input deck-size card shuffles)
  (let ((memory (make-array 2 :initial-contents (list 1 0))))
    (loop for line in (reverse input) 
          for x from 0
          do
          (progn
            (cond
              ((is-command line +deal-with-increment+)
               (let* ((num (increment-num line))
                      (val (mod-pow num (- deck-size 2) deck-size)))

                 (setf (aref memory 0) (* (aref memory 0) val))
                 (setf (aref memory 1) (* (aref memory 1) val))))
              ((is-command line +deal-into-new-stack+)
               (setf (aref memory 0) (- (aref memory 0)))
               (setf (aref memory 1) (- (1+ (aref memory 1)))))
              ((is-command line +cut+)
               (let ((num (get-num line +cut+)))
                 (setf (aref memory 1) (+ (aref memory 1) num)))))

            (setf (aref memory 0) (mod (aref memory 0) deck-size))
            (setf (aref memory 1) (mod (aref memory 1) deck-size))))

    ;; Black magic math go brrr
    (let* ((power (mod-pow (aref memory 0) shuffles deck-size))
           (pow-find (* power card))
           (pow-plus-dec-num (+ power (1- deck-size)))
           (aux (* (aref memory 1) pow-plus-dec-num))
           (pow-min-1 (mod-pow (1- (aref memory 0)) (- deck-size 2) deck-size))
           (res (mod (+ pow-find (* aux pow-min-1)) deck-size)))
      res)))


(defun part-one (input)
  (position 2019 (shuffle input 10007)))


(defconstant +part-two-deck-size+ 119315717514047)
(defconstant +shuffles+ 101741582076661)

(defun part-two (input)
  (get-number 
    input 
    +part-two-deck-size+
    2020
    +shuffles+))

(defun main()
  (let ((input (get-input (input))))
    (my-timer 'Silver #'part-one input)
    (my-timer 'Gold #'part-two input)))
