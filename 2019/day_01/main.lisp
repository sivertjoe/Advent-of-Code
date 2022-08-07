(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")

(defun get-input (filename)
  (->> filename
    (read-lines)
    (mapcar 'parse-integer)))

(defun part-one ()
  (defun helper (acc x)
    (+ acc (- (floor x 3) 2)))
  (reduce #'helper (get-input "input") :initial-value 0))

(defun part-two ()
  (defun helper (acc x)
      (let ((res  (- (floor x 3) 2)))
        (if (<= res 0)
          0
          (+ acc res (helper 0 res)))))
  (reduce #'helper (get-input "input") :initial-value 0))

(defun main()
    (format t "~a~%" (part-one))
    (format t "~a~%" (part-two)))

(main)
