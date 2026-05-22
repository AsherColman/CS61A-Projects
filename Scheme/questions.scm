(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cadar x) (car (cdr (car x))))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

;; Problem 14
;; Returns a list of two-element lists
(define (enumerate s)
  (define (helper s i)
    (if (null? s)
        nil
        (cons (list i (car s))
              (helper (cdr s) (+ i 1)))))
  (helper s 0)
)


;; Problem 15

;; Return the value for a key in a dictionary list
(define (get dict key)
  (cond
    ((null? dict) #f)
    ((equal? (car (car dict)) key)
     (car (cdr (car dict))))
    (else (get (cdr dict) key)))
)

;; Return a dictionary list with a (key value) pair
(define (set dict key val)
  (cond
    ((null? dict)
     (cons (list key val) nil))

    ((equal? (car (car dict)) key)
     (cons (list key val)
           (cdr dict)))

    (else
     (cons (car dict)
           (set (cdr dict) key val))))
)

;; Problem 16

;; implement solution-code
(define (solution-code problem solution)
  (cond
    ((null? problem) nil)

    ((equal? (car problem) '_____)
     (cons solution
           (solution-code (cdr problem) solution)))

    ((pair? (car problem))
     (cons (solution-code (car problem) solution)
           (solution-code (cdr problem) solution)))

    (else
     (cons (car problem)
           (solution-code (cdr problem) solution))))
)
