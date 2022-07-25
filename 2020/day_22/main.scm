(define (readlines filename)
  (call-with-input-file filename
    (lambda (p)
      (let loop ((line (read-line p))
                 (result '()))
        (if (eof-object? line)
            (reverse result)
            (loop (read-line p) (cons line result)))))))

(define (split-list lst)
  (define (helper lst global curr)
    (if (null? lst)
      (cons curr global)
      (let ((line (car lst)))
        (cond
          ((string<=? "Player" line)
            (helper (cdr lst) global curr))
          ((= (string-length line) 0)
           (helper (cdr lst) (cons curr global) '()))
          (#t
           (helper (cdr lst) global (append curr (list (string->number line)))))))))
  (helper lst '() '()))

(define (read-input)
  (split-list (readlines "input")))

(define (sum lst)
  (define (helper x acc)
    (list 
      (+ (car acc) (* x (cadr acc))) 
      (- (cadr acc) 1)))

  (car (fold helper (list 0 (length lst)) lst)))


(define (p1-won-normal p1-top p1-deck p2-top p2-deck func)
  (> p1-top p2-top))

(define (p1-won-round p1-top p1-deck p2-top p2-deck func)
  (if (or (< (length p1-deck) p1-top) (< (length p2-deck) p2-top))
    (> p1-top p2-top)
    (cadr (recursive-combat (take p1-deck p1-top) (take p2-deck p2-top) func))))

(define (recursive-combat p1-deck p2-deck func)
  (define (helper p1-deck p2-deck history func)
    (cond
      ((or (member p1-deck history) (member p2-deck history))
       (list p1-deck #t))
      ((null? p1-deck)
       (list p2-deck #f))
      ((null? p2-deck)
       (list p1-deck #t))
      (else
        (let ((p1-top (car p1-deck))
              (p2-top (car p2-deck))
              (new-history (cons p1-deck (cons p2-deck history))))
          (if (func p1-top (cdr p1-deck) p2-top (cdr p2-deck) func)
            (helper (append (cdr p1-deck) (list p1-top p2-top)) (cdr p2-deck) new-history func)
            (helper (cdr p1-deck) (append (cdr p2-deck) (list p2-top p1-top)) new-history func))))))
  (helper p1-deck p2-deck '() func))

(define (play-combat rule)
  (let* ((decks (read-input))
         (p2-deck (car decks))
         (p1-deck (cadr decks)))
    (sum (car (recursive-combat p1-deck p2-deck rule)))))

(define (part-one)
  (play-combat p1-won-normal))

(define (part-two)
  (play-combat p1-won-round))

(begin
  (display (part-one)) 
  (newline)
  (display (part-two)) 
  (newline))
