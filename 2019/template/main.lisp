(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")
(load "../util/function.lisp")

(defun get-input (filename)
  (->> filename
    (read-lines)))

(defun part-one (input)
  0)

(defun part-two (input)
  0)

(defun main()
  (let ((input (get-input (input))))
      (my-timer 'Silver #'part-one input)
      (my-timer 'Gold #'part-two input)))
