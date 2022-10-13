;; Note: This code is very poorly written. It is very difficult to understand 
;; and read; although it produces the correct output. Preceed with caution.

(load "../util/file.lisp")
(load "../util/arrows.lisp")
(load "../util/string.lisp")
(load "../util/template.lisp")
(load "../util/function.lisp")
(load "../util/queue.lisp")
(load "../util/heap.lisp")

(defun get-input (filename)
  (->> filename
       (read-lines)))

(defun key-val (ht)
  (loop for k being the hash-keys in ht using (hash-value v)
        collect (list k v)))

(defconstant +wall+ #\#)
(defconstant +empty+ #\.)

(defun create-grid (input)
  (let ((map (make-hash-table :test 'equal))
        (y 0)
        (x 0))
    (loop for line in input do
          (progn
            (setf x 0)
            (loop for c across line do 
                  (progn
                    (setf (gethash (list x y) map) c)
                    (incf x)))
            (incf y)))

    map))

(defun neighbors (point)
  (list
    (list (1+ (car point)) (cadr point))
    (list (1- (car point)) (cadr point))
    (list (car point) (1+ (cadr point)))
    (list (car point) (1- (cadr point)))))

(defun reachable-from (grid coord)
  (let ((visited (make-hash-table :test 'equal))
        (result (make-hash-table :test 'equal))
        (q (make-queue :initial-contents (list (list coord 0)))))
    (setf (gethash coord visited ) t)
    (loop
      for elm = (queue-pop q)
      while elm
      finally (return result)
      do (destructuring-bind (current steps) elm
           (loop for neighbor in (neighbors current) do
                 (progn
                   (let ((tile (gethash neighbor grid)))
                     (when (and 
                             tile 
                             (not (gethash neighbor visited)))
                       (setf (gethash neighbor visited) t)
                       (cond
                         ((char= tile +empty+)
                          (queue-push (list neighbor (1+ steps)) q))
                         ((not (char= tile +wall+))
                          (setf (gethash tile result) (1+ steps))))))))))))

(defun is-node (tile)
  (and (not (char= tile +wall+)) (not (char= tile +empty+))))

(defun create-graph (grid)
  (let ((graph (make-hash-table :test 'equal)))
    (loop 
      for kv in (key-val grid) 
      finally (return graph)
      do
      (destructuring-bind (coord tile) kv
        (when (is-node tile)
          (let ((pos-edges (reachable-from grid coord)))
            (setf (gethash tile graph) pos-edges)))))))

(defun add-d (x y)
  (declare (type (unsigned-byte 64) x y))
  (logand (+ x y) #xfffffffffffffff))

;; Note: This is terrible. In the algorithm I need to use (char, set<char>) 
;; as a hashmap key. Therefore I need to implement the equality function AND 
;; hash function, in order to use (char, set<char>) as a key.
(defun ht-equality-fn (key1 key2)
  (and (equalp (cadr key1) (cadr key2)) (char= (car key1) (car key2))))

(defun ht-hash-fn (val)
  (let ((sum 0))
    (maphash (lambda (key value)
               (setf sum (add-d sum (sxhash key))))
             (cadr val))
    (add-d sum (sxhash (car val)))))

(sb-ext:define-hash-table-test ht-equality-fn ht-hash-fn)
;; end note

(defun g-steps (a)
  (car a))
(defun g-keys (a)
  (third a))
(defun g-node (a)
  (second a))
(defun len (a)
  (hash-table-count a))

(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table 
              :test (hash-table-test hash-table)
              :rehash-size (hash-table-rehash-size hash-table)
              :rehash-threshold (hash-table-rehash-threshold hash-table)
              :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
          using (hash-value value)
          do (setf (gethash key ht) value)
          finally (return ht))))

(defun compare-heap-elem (a b)
  (if (= (g-steps a) (g-steps b))
    (< (len (g-keys a)) (len (g-keys b)))
    (< (g-steps a) (g-steps b))))

(defun skip-larger-p (best-steps current)
  (and best-steps (> best-steps (g-steps current))))

(defun search-graph (graph start)
  (let ((heap (make-heap 
                #'compare-heap-elem 
                :initial-contents (list (list 0 start (make-hash-table)))))
        (key-count (length (remove-if 
                             (lambda (x) (not (lower-case-p (car x))))
                             (key-val graph))))
        (distances (make-hash-table :test 'ht-equality-fn))
        (cache (make-hash-table :test 'ht-equality-fn)))

    (setf (gethash (list start (make-hash-table)) distances) 0)

    (loop
      for current = (heap-pop heap) 
      while current
      do (progn
           (if (= (len (g-keys current)) key-count)
             (return (g-steps current))
             (progn
               (when (not (skip-larger-p (gethash (list 
                                                    (g-node current)
                                                    (g-keys current))
                                                  distances)
                                         current))
                 (let ((cache-key (list (g-node current) (g-keys current))))
                   (unless (gethash cache-key cache)
                     (setf (gethash cache-key cache) 
                           (search-keys graph 
                                        (g-keys current) 
                                        (g-node current))))
                   (loop for (next-node cost) (character integer) in
                         (gethash cache-key cache) do
                         (let ((next-keys (copy-hash-table (g-keys current)))
                               (next-steps (+ (g-steps current) cost)))
                           (setf (gethash next-node next-keys) t)
                           (unless (gethash (list next-node next-keys) distances)
                             (setf (gethash (list next-node next-keys) distances)
                                   most-positive-fixnum))
                           (let ((distances-entry 
                                   (gethash (list next-node next-keys) distances)))
                             (when (< next-steps distances-entry)
                               (setf (gethash (list next-node next-keys) distances)
                                     next-steps)
                               (heap-push
                                 (list (+ (g-steps current) cost)
                                       next-node
                                       next-keys)
                                 heap)))))))))))))

(defun g-cost (a)
  (car a))
(defun g-current (a)
  (cadr a))

(defun compare-dijkstra (a b)
  (if (= (g-cost a) (g-cost b))
    (char< (g-current a) (g-current b))
    (< (g-cost a) (g-cost b))))

(defun search-keys (graph keys start)
  (let ((dist (make-hash-table))
        (heap (make-heap 'compare-dijkstra ))
        (reach (make-hash-table)))

    (loop for key in (hash-keys graph)
          do (setf (gethash key dist) most-positive-fixnum))

    (setf (gethash start dist) 0)
    (heap-push (list 0 start) heap)
    (loop
      for dijk = (heap-pop heap)
      while dijk
      do (destructuring-bind (cost current) dijk
           (cond
             ((and (lower-case-p current) (not (gethash current keys)))
              (setf (gethash current reach) t))
             ((> cost (gethash current dist))
              '())
             (t
               (loop for (next-node next-cost) (character integer) in
                     (key-val (gethash current graph)) do
                     (when (or (not (upper-case-p next-node)) (gethash (char-downcase next-node) keys))
                       (let ((next (list (+ cost next-cost) next-node)))
                         (when (< (g-cost next) (gethash next-node dist))
                           (setf (gethash next-node dist) (g-cost next))
                           (heap-push next heap)))))))))

    (mapcar 
      (lambda (node) (list node (gethash node dist)))
      (hash-keys reach))))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun find-target (ht target)
  (loop 
    for kv in (key-val ht)
    when (char= (cadr kv) target)
    return (car kv)))

(defun four-robots (grid)
  (let ((robot-coord (find-target grid #\@)))
    (setf (gethash robot-coord grid) +wall+)
    (loop for neighbor in (neighbors robot-coord) do
          (setf (gethash neighbor grid) +wall+))
    (setf (gethash 
            (list (1- (car robot-coord)) (1- (cadr robot-coord)))
            grid)
          #\@)

    (setf (gethash 
            (list (1- (car robot-coord)) (1+ (cadr robot-coord)))
            grid)
          #\=)

    (setf (gethash 
            (list (1+ (car robot-coord)) (1+ (cadr robot-coord)))
            grid)
          #\%)

    (setf (gethash 
            (list (1+ (car robot-coord)) (1- (cadr robot-coord)))
            grid)
          #\$)

    grid))


(defun ht-equality-fn2 (key1 key2)
  (and (equalp (cadr key1) (cadr key2)) (equalp (car key1) (car key2))))

(defun ht-hash-fn2 (val)
  (let ((sum 0))
    (maphash (lambda (key value)
               ;; Note: tinker with this value
               (setf sum (add-d sum (add-d (sxhash key) (sxhash value)))))
             (cadr val))
    (loop for c in (car val) do
          (setf sum (add-d sum (sxhash c))))
    sum))
(sb-ext:define-hash-table-test ht-equality-fn2 ht-hash-fn2)

(defun compare-four-state (a b)
  (if (= (f-steps a) (f-steps b))
    (< (len (f-keys a)) (len (f-keys b)))
    (< (f-steps a) (f-steps b))))

(defun f-steps (a)
  (car a))
(defun f-robots (a)
  (second a))
(defun f-keys (a)
  (third a))

(defun search-four (graph)
  (let ((heap (make-heap 'compare-four-state))
        (key-count (length (remove-if 
                             (lambda (x) (not (lower-case-p (car x))))
                             (key-val graph))))
        (dist (make-hash-table :test 'ht-equality-fn2))
        (robots (list #\@ #\= #\% #\$))
        (cache (make-hash-table :test 'ht-equality-fn)))
    (setf (gethash (list (copy-list robots) (make-hash-table)) dist) 0)
    (heap-push
      (list 
        0                  ; steps
        (copy-list robots) ; robots
        (make-hash-table)) ; keys
      heap)

    (loop
      for current = (heap-pop heap) 
      while current
      do (progn
           (if (= (len (f-keys current)) key-count)
             (return (f-steps current))
             (let ((best-steps (gethash (list (f-robots current) (f-keys current)) dist)))
               (when (or (not best-steps) (<= (f-steps current) best-steps))
                 (loop for robot-location in (f-robots current)
                       for robot-number from 0 do
                       (progn
                         (let ((cache-key (list robot-location (f-keys current))))
                           (unless (gethash cache-key cache)
                             (setf (gethash cache-key cache) (search-keys graph 
                                                                          (f-keys current)
                                                                          robot-location)))
                           (let ((cached-entry (gethash cache-key cache)))
                             (loop for (next-node cost) (character integer) in 
                                   (gethash cache-key cache) do
                                   (let ((next-keys (copy-hash-table (f-keys current)))
                                         (next-robots (copy-list (f-robots current)))
                                         (next-steps (+ (f-steps current) cost)))
                                     (setf (gethash next-node next-keys) t)
                                     (setf (nth robot-number next-robots) next-node)
                                     (unless (gethash (list next-robots next-keys) dist)
                                       (setf (gethash (list next-robots next-keys) dist)
                                             most-positive-fixnum))
                                     (when (< next-steps 
                                              (gethash (list next-robots next-keys) dist))
                                       (setf (gethash (list next-robots next-keys) dist)
                                             next-steps)
                                       (heap-push
                                         (list next-steps next-robots next-keys)
                                         heap)))))))))))))))

(defun part-one (input)
  (-> input
      (create-grid)
      (create-graph)
      (search-graph #\@)))

(defun part-two (input)
  (-> input
      (create-grid)
      (four-robots)
      (create-graph)
      (search-four)))

(defun main()
  (let ((input (get-input (input))))
    (my-timer 'Silver #'part-one input)
    (my-timer 'Gold #'part-two input)))
