;;; Note: _A LOT_ of inline and type hints have been added to the code to reduce the
;;; Execution time from 1s to 0.4 

(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")
(load "../util/queue.lisp")

(defun get-input (filename)
  (->> filename
       (read-lines)))

(declaim (ftype (function (fixnum fixnum) fixnum) index))
(defun index (x y)
  (declare 
    (type fixnum x y)
    (optimize (speed 3)))
  (+ (* y 5) x))

(defun parse (input)
  (let ((array (make-array 25 :element-type 'bit)))
    (loop for line in input
          for y from 0
          do
          (loop for ch across line
                for x from 0
                do
                (setf
                  (aref array (index x y))
                  (if (char= ch #\#) 1 0))))
    array))

(defconstant +bug+ 1)
(defconstant +empty+ 0)

(defun count-bugs (index array)
  (let ((x (car (to-point index)))
        (y (cadr (to-point index))))
    (-<>> 
      (list
        (list (1+ x) y)
        (list (1- x) y)
        (list x (1+ y))
        (list x (1- y)))
      (remove-if-not (lambda (x) 
                       (every (lambda (idx) (and (>= idx 0) (< idx 5))) x)))
      (mapcar (lambda (x) (aref array (index (car x) (cadr x)))))
      (loop for bug in <> sum bug))))

(defun next-cell (bug bug-count)
  (cond
    ((and (= bug +bug+) (/= bug-count 1))
     +empty+)
    ((and (= bug +empty+) (or (= bug-count 1) (= bug-count 2)))
     +bug+)
    (t
      bug)))

(declaim (inline empty-bit-set))
(defun empty-bit-set ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (make-array 25 :element-type 'bit))

(defun next (array)
  (let ((new (make-array 25 :element-type 'bit)))
    (loop for bug across array
          for index from 0
          for bug-count = (count-bugs index array)
          for next-cell = (next-cell bug bug-count)
          do
          (setf (aref new index) next-cell))
    new))

(defun pretty (array)
  (loop for bug across array
        for index from 0 do
        (progn
          (when (= (mod index 5) 0)
            (format t "~%"))
          (format t "~a" (if (= bug +bug+) #\# #\.))))
  (format t "~%"))

(defun appear-twice (state)
  (let ((seen (make-hash-table :test 'equal)))
    (setf (gethash state seen) t)
    (loop for new = (next state)
          when (gethash new seen)
          return new
          do
          (progn
            (setf state new)
            (setf (gethash state seen) t)))))

(defun bio-diversity-rating (array)
  (loop for bug across array
        for pow from 0
        sum (* (expt 2 pow) bug)))

(defun to-point (idx)
  (list (mod idx 5) (floor idx 5)))

(defconstant +ns+
             (list
               (list 0 -1)
               (list -1 0)
               (list 0 1)
               (list 1 0)))

(declaim (inline point-add))
(defun point-add (x y p2)
  (declare (type fixnum x y)
           (optimize (speed 3)))
  (list (+ x (car p2)) (+ y (cadr p2))))

(declaim (inline Y))
(defun Y (p)
  (cadr p))

(declaim (inline X))
(defun X (p)
  (car p))

;; This function _really_ needs to burn
(declaim (inline empty-bit-set))
(defun w-idx (i x y)
  (declare 
    (type fixnum i x y)
    (optimize (speed 3) (debug 0) (safety 0)))
  (abs (- (* (logand x 1) i)
          (* (* (logand y 1) 2)
             (- y 1)))))

(declaim (inline count-neighbors))
(defun count-neighbors (grid x y z)
  (declare 
    (type hash-table grid)
    (type fixnum x y z)
    (optimize (speed 3) (debug 0) (safety 0)))
  (let ((adj 0))
    (loop for d in +ns+ do
          (let ((n (point-add x y d)))
            (cond
              ((or (= (Y n) -1) (= (Y n) 5) (= (X n) -1) (= (X n) 5))
               (unless (gethash (1- z) grid)
                 (setf (gethash (1- z) grid) (empty-bit-set)))
               (incf adj 
                     (aref (gethash (1- z) grid)
                           (index (+ (X d) 2) (+ (Y d) 2)))))
              ((and (= (Y n) 2) (= (X n) 2))
               (loop for ii from 0 below 5 do
                     (progn
                       (unless (gethash (1+ z) grid)
                         (setf (gethash (1+ z) grid) (empty-bit-set)))
                       
                       (incf adj
                             (aref 
                               (gethash (1+ z) grid)
                               (index (w-idx ii y x) (w-idx ii x y)))))))
              (t
                (unless (gethash z grid)
                  (setf (gethash z grid) (empty-bit-set)))
                (incf adj 
                      (aref (gethash z grid) (index (X n) (Y n))))))))
    adj))

(defun solve (input n)
  (let ((init (parse input))
        (grid (make-hash-table))
        (bugs 0))
    (setf (gethash 0 grid) init)
    (loop for i from 0 below n do
          (let  ((next (make-hash-table)))
            (setf bugs 0)
            (loop for z from (- n) to n do
                  (loop for y from 0 below 5 do
                        (loop for x from 0 below 5 do
                              (when (not (and (= y 2) (= x 2)))
                                (let ((adj (count-neighbors grid x y z)))
                                  (unless (gethash z grid)
                                    (setf (gethash z grid) (empty-bit-set)))
                                  (when (or
                                          (= adj 1)
                                          (and
                                            (= (aref (gethash z grid) (index x y)) 0)
                                            (= adj 2)))
                                    (unless (gethash z next)
                                      (setf (gethash z next) (empty-bit-set)))
                                    (setf (aref (gethash z next) (index x y)) 1)
                                    (incf bugs)))))))
            (setf grid next)))
    bugs))

(defun part-one (input)
  (->
    input
    (parse)
    (appear-twice)
    (bio-diversity-rating)))

(defun part-two (input)
  (solve input 200))

(defun main()
  (let ((input (get-input (input))))
    (my-timer 'Silver #'part-one input)
    (my-timer 'Gold #'part-two input)))
