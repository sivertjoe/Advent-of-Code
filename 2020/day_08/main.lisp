(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun parse-line (line)
    (let* ((line (split-by-one-space line))
           (num (second line)))
            (setf (nth 1 line) (parse-integer num))
            line))

(defun get-file (filename)
    (with-open-file (stream filename)
        (loop for line = (read-line stream nil)
            while line
              collect (parse-line line))))


(defvar *lines* (get-file "input"))

(defun find-inf- (acc lines i table)
    (if (>= i (length lines))
        (values acc t) ; End of the program!
        (let* ((hash (gethash i table)) 
               (line (nth i lines))
               (cmd (car line))
               (val (second line)))
            (if hash
                (values acc nil) ; We have seen this line before
                (progn
                    (setf (gethash i table) t)
                    (cond 
                        ((string= cmd "acc")
                            (find-inf- (+ acc val) lines (+ i 1) table))
                        ((string= cmd "nop")
                            (find-inf- acc lines (+ i 1) table))
                        ((string= cmd "jmp")
                            (find-inf- acc lines (+ i val) table))))))))


(defun get-other (cmd)
    (if (string= cmd "jmp")
        "nop"
        "jmp"))

(defun find-inf (lines)
    (find-inf- 0 lines 0 (make-hash-table)))

(defun find-corrupted (lines curr)
    (let* ((line (nth curr lines))
           (cmd (car line)))
        (if (string= cmd "acc")
            (find-corrupted lines (+ 1 curr)) ; Skip acc instructions
            (let ((new (get-other cmd)))
                (setf (car (nth curr lines)) new) ; Change the nop/jmp
                (multiple-value-bind (acc exit)
                    (find-inf lines) ; Check if it terminates or loops
                    (if exit
                        acc
                        (progn
                            (setf (car (nth curr lines)) cmd)
                            (find-corrupted lines (+ 1 curr)))))))))

(defun part-one ()
    (find-inf *lines*))

(defun part-two ()
    (find-corrupted *lines* 0))

(defun main()
    (format t "~a~%" (part-one))
    (format t "~a~%" (part-two)))

(main)