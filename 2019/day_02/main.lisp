(load "../util/file.lisp")
(load "../util/arrows.lisp")

(defun read-input (filename)
  (->> filename
    (read-lines)
    (car)
    (split-str ",")
    (mapcar 'parse-integer)))
  
(defun get-num (array index)
  (aref array (aref array index)))

(defun handle (array index op)
  (let* ((fst (get-num array (+ index 1)))
        (snd (get-num array (+ index 2)))
        (res (funcall op fst snd)))
    (setf (aref array (aref array (+ index 3))) res))
    (run array (+ index 4)))

(defun run (array index)
  (let ((curr (aref array index)))
    (cond
      ((= curr 99)
       (aref array 0))
      ((= curr 1)
       (handle array index '+))
      ((= curr 2)
       (handle array index '*)))))

(defun get-array ()
  (let ((lines (read-input "input")))
    (make-array (length lines) :initial-contents lines)))

(defun compute-nouns (array n1 n2)
  (setf (aref array 1) n1)
  (setf (aref array 2) n2)
  (run array 0))

(defun part-one ()
  (compute-nouns (get-array) 12 2))

(defun part-two ()
  (let ((array (get-array)))
    (loop for a from 1 below 100 do
      (loop for b from 1 below 100
        if (= (compute-nouns (copy-seq array) a b) 19690720) 
          do (return-from part-two (+ (* 100 a) b))))))

(defun main()
    (format t "~a~%" (part-one))
    (format t "~a~%" (part-two)))

(main)
