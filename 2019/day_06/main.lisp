(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")

(defun get-input (filename)
  (->> filename
    (read-lines)))

(defun parse (input)
  (->> input
       (mapcar #'(lambda (s) (split-str ")" s)))))

(defun build (map list)
  (cond
    ((null list)
     map)
    (t
      (setf (gethash (cadr (car list)) map) (caar list))
      (build map (cdr list)))))

(defun hash-keys (map)
  (loop for k being each hash-key of map
       collect k))

(defun count-orbit (map acc x)
  (let ((hash (gethash x map)))
    (cond 
      ((equal hash "COM")
       (+ acc 1))
      (t
        (+ 1 (count-orbit map acc hash))))))

(defun part-one (input)
  (let ((map (build (make-hash-table :test 'equal) (parse input))))
    (flet ((helper (acc x) (count-orbit map acc x)))
        (reduce #'helper (hash-keys map) :initial-value 0))))

(defun build-path (map k)
  (let ((hash (gethash k map)))
    (if (not hash) (format t "~a~%" hash))
    (cond 
      ((equal hash "COM")
       '())
      (t
        (nconc (build-path map hash) (list hash))))))

(defun find-common (map)
  (labels ((diff (d1 d2)
    (cond 
        ((not (equal (car d1) (car d2)))
         (+ 2 (length (cdr d1)) (length (cdr d2)))) 
        (t
          (diff (cdr d1) (cdr d2))))))
  (diff (build-path map "YOU") (build-path map "SAN"))))

(defun part-two (input)
  (->> input
        (parse)
        (build (make-hash-table :test 'equal))
        (find-common)))

(defun main()
  (let ((input (get-input (input))))
      (my-timer 'Silver #'part-one input)
      (my-timer 'Gold #'part-two input)))
