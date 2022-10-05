(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")

(defun get-input (filename)
  (->> filename
       (read-lines)
       (mapcar
         (lambda (x) (split-str "=>" x)))
       (mapcar
         (lambda (x) (remove-if (lambda (s) (= (length s) 0)) x)))
       (mapcar
         (lambda (x)
           (mapcar
             (lambda (s)
               (string-trim (list #\Space #\Newline) s)) x)))
       (mapcar
         (lambda (x) (list
                       (map-input (car x))
                       (parse-ing (cadr x)))))))

(defun trim (s)
  (string-trim (list #\Space #\Newline) s))

(defun parse-ing (s)
  (let ((spl (split-str " " (trim s))))
    (list (parse-integer (car spl)) (cadr spl))))

(defun map-input (s)
  (->> s
       (split-str ",")
       (mapcar 'parse-ing)))

(defun create-map (lst)
  (cond
    ((null lst)
     (make-hash-table :test 'equal))
    (t
      (let ((ht (create-map (cdr lst))))
        (setf (gethash (second (second (car lst))) ht)
              ;  The ingredients           the out amount
              (list (car (car lst)) (car (second (car lst)))))
        ht))))

(defun keys (ht)
  (loop for k being each hash-key of ht
        collect k))

(defun get-num-reactions (num-needed num-produced)
  (if (= (/ num-needed num-produced) (floor num-needed num-produced))
    (floor num-needed num-produced)
    (1+ (floor num-needed num-produced))))

(defun determin (needs have ore ht)
  (if (= (hash-table-count needs) 0)
    (car ore)
    (let ((item (car (keys needs))))
      (if (<= (gethash item needs) (gethash item have 0))
        (progn
          (setf
            (gethash item have)
            (- (gethash item have) (gethash item needs)))
          (remhash item needs)
          (determin needs have ore ht))
        (let ((num-needed (- (gethash item needs) (gethash item have 0))))
          (remhash item have)
          (remhash item needs)
          (let ((num-produced (second (gethash item ht))))
            (let ((num-reactions (get-num-reactions num-needed num-produced)))
              (setf (gethash item have) (+ (gethash item have 0)
                                           (- (* num-reactions num-produced)
                                              num-needed)))
              (loop for chem in (car (gethash item ht)) do
                    (if (string= (cadr chem) "ORE")
                      (setf (car ore) (+ (car ore)
                                         (* (car chem) num-reactions)))
                      (setf (gethash (cadr chem) needs)
                            (+ (gethash (cadr chem) needs 0)
                               (* (car chem) num-reactions)))))
              (determin needs have ore ht))))))))

(defun fuel-cost (ht &optional (fuel 1))
  (let ((needs (make-hash-table :test 'equal))
        (ore (list 0)))
    (setf (gethash "FUEL" needs) fuel)
    (determin needs (make-hash-table :test 'equal) ore ht)))

(defun part-one (input)
  (let ((ht (create-map input)))
    (fuel-cost ht)))

(defparameter *limit* 1000000000000)

(defun part-two (input)
  (let ((ht (create-map input)))
      (let* ((low (floor *limit*  (fuel-cost ht)))
             (high (* 10 low)))
        (loop
          when (>= (fuel-cost ht high) *limit*)
          return '()
          do (progn
               (setf low high)
               (setf high (* low 10))))
        (let* ((mid 0)
               (ore 0))
          (loop
            when (>= low (1- high))
            return (1- mid)
            do (progn
                 (setf mid (floor (+ low high) 2))
                 (setf ore (fuel-cost ht mid))
                 (cond 
                   ((< ore *limit*)
                    (setf low mid))
                   ((> ore *limit*)
                    (setf high mid)))))))))

(defun main()
  (let ((input (get-input (input))))
    (my-timer 'Silver #'part-one input)
    (my-timer 'Gold #'part-two input)))
