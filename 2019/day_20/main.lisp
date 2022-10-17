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

(defconstant +open+ #\.)

(defun neighbors (point)
  (list
    (list (1+ (car point)) (cadr point))
    (list (1- (car point)) (cadr point))
    (list (car point) (1+ (cadr point)))
    (list (car point) (1- (cadr point)))))

(defun letters (ch)
  (alpha-char-p ch))

(defun parse-maze (input)
  (let ((maze (make-hash-table :test 'equal)))
    (loop for line in input 
          for y from 0 do
          (loop for ch across line
                for x from 0 do
                (when (char= ch +open+)
                  (setf (gethash (list x y) maze) t))))
    maze))

(defun east (point)
  (list (1+ (car point)) (cadr point)))
(defun west (point)
  (list (1- (car point)) (cadr point)))

(defun north (point)
  (list (car point) (1- (cadr point))))
(defun south (point)
  (list (car point) (1+ (cadr point))))

(defun get-or-null (input point)
  (if (and
        (and (< (cadr point) (length input)) (>= (cadr point) 0))
        (and (< (car point) (length (nth (cadr point) input))) (>= (car point) 0)))
    (aref (nth (cadr point) input) (car point))
    #\!)) ;; Hacky !

(defun link-portals (input maze)
  (let ((linked (make-hash-table :test 'equal))
        (unlinked (make-hash-table :test 'equal)))
    (loop for line in input
          for y from 0 do
          (loop for c across line
                for x from 0 do
                (when (alpha-char-p c)
                  (let ((at (list x y))
                        (name nil)
                        (portal nil))
                    (cond
                      ((and 
                         (alpha-char-p (get-or-null input (south at)))
                         (gethash (north at) maze))
                       (setf name (format nil "~a~a" c (get-or-null input (south at))))
                       (setf portal (north at)))

                      ((and 
                         (alpha-char-p (get-or-null input (south at)))
                         (gethash (south (south at)) maze))
                       (setf name (format nil "~a~a" c (get-or-null input (south at))))
                       (setf portal (south (south at))))

                      ((and 
                         (alpha-char-p (get-or-null input (east at)))
                         (gethash (west at) maze))
                       (setf name (format nil "~a~a" c (get-or-null input (east at))))
                       (setf portal (west at)))

                      ((and 
                         (alpha-char-p (get-or-null input (east at)))
                         (gethash (east (east at)) maze))
                       (setf name (format nil "~a~a" c (get-or-null input (east at))))
                       (setf portal (east (east at)))))

                    (when (and name portal)
                      (if (gethash name unlinked)
                        (progn
                          (setf (gethash portal linked) (gethash name unlinked))
                          (setf (gethash (gethash name unlinked) linked) portal)
                          (remhash name unlinked))
                        (setf (gethash name unlinked) portal)))))))
    (values linked (gethash "AA" unlinked) (gethash "ZZ" unlinked))))


(defun add-portals (point portals neighbors)
  (if (gethash point portals)
    (cons (gethash point portals) neighbors)
    neighbors))

(defun maze-neighbors (node maze portals)
  ( ->> (car node)
        (neighbors)
        (remove-if-not (lambda (x) (gethash x maze)))
        (add-portals (car node) portals)))

(defun part-one (input)
  (let ((maze (parse-maze input)))
    (multiple-value-bind (portals begin end) (link-portals input maze)
      (flet ((neighbors (node)
                        (maze-neighbors node maze portals)))
        (bfs 
          :from begin 
          :end end 
          :neighbors #'neighbors 
          :test #'equal)))))

(defun add-portals2 (node portals outward neighbors)
  (let ((point (car node)))
    (if (gethash point portals)
      (let ((level-diff (if (or (find (car point) (car outward))
                                (find (cadr point) (cadr outward)))
                          -1
                          1)))
        (cons (list (gethash point portals) (+ (second node) level-diff)) neighbors))
      neighbors)))

(defun maze-neighbors2 (node maze portals outwards)
  (->> 
    (neighbors (caar node))
    (remove-if-not (lambda (x) (gethash x maze)))
    (mapcar (lambda (x) (list x (cadr (car node)))))
    (add-portals2 (car node) portals outwards)
    (remove-if-not (lambda (x) (>= (second x) 0)))))

(defun part-two (input)
  (let ((maze (parse-maze input)))
    (multiple-value-bind (portals begin end) (link-portals input maze)
      (let ((outwards (list (list 2 (- (length (car input)) 3))
                            (list 2 (- (length input) 3)))))
        (flet ((neighbors (node)
                          (maze-neighbors2 node maze portals outwards)))
          (bfs 
            :from (list begin 0) 
            :end (list end 0) 
            :neighbors #'neighbors 
            :test #'equal))))))

(defun main()
  (let ((input (get-input (input))))
    (my-timer 'Silver #'part-one input)
    (my-timer 'Gold #'part-two input)))
