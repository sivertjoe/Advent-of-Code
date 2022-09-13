(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")

(defun get-input (filename)
  (-> filename
    (read-lines)
    (car)
    (coerce 'list)))

(defparameter *w* 25)
(defparameter *h* 6)


(defun split-into-layers (array n) 
  (labels ((helper (array i n buff)
      (cond
        ((null array)
         '())
        ((= i n)
         (cons 
           (cons (car array) buff) 
           (helper (cdr array) 1 n '())))
        (t
          (helper (cdr array) (1+ i) n (cons (car array) buff))))))
    (helper array 1 n '())))


(defun layer-with-fewest-zeros (array w h)
  (reduce 
    (lambda (acc x)
      (if (< (count #\0 acc) (count #\0 x))
        acc
        x))
    (split-into-layers array (* w h))))

(defun part-one (input)
  (let ((layer (layer-with-fewest-zeros input *w* *h*)))
    (* (count #\1 layer) (count #\2 layer))))

(defmacro zip (a b)
  `(mapcar #'list ,a ,b))

(defvar *black* #\0)
(defvar *white* #\1)
(defvar *transparent* #\2)


(defun join (l)
  (mapcar
    (lambda (x)
      (let ((a (car x))
            (b (cadr x)))
        (if (eql a *transparent*)
          b
          a)))
    l))

(defun display (array w)
  (->> (split-into-layers array w)
       (mapcar (lambda (x) (coerce x 'string)))
       (reverse)
       (cons #\NewLine)
       (reduce (lambda (acc x) (format nil "~a~%~a" acc x)))
       (map 'string (lambda (c)
                 (cond
                   ((eql c *transparent*)
                    #\x)
                   ((eql c *white*)
                    #\#)
                   ((eql c *black*)
                    #\SPACE)
                   (t
                     c))))))

(defun part-two (input)
  (display 
    (reduce
        (lambda (acc x) (join (zip acc x)))
        (split-into-layers input (* *w* *h*)))
    *w*))

(defun main()
  (let ((input (get-input (input))))
      (my-timer 'Silver #'part-one input)
      (my-timer 'Gold #'part-two input)))
