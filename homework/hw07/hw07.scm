(define (filter-lst fn lst)
  (cond
    ((null? lst) nil)
    ((fn (car lst)) 
     (cons (car lst) (filter-lst fn (cdr lst))))
    (else (filter-lst fn (cdr lst)))
  )
)

;;; Tests
(define (even? x)
  (= (modulo x 2) 0))
(filter-lst even? '(0 1 1 2 3 5 8))
; expect (0 2 8)


(define (interleave first second)
  (interleaves 1 first second)
)

(define (interleaves which first second)
  (cond
    ((null? first) second)
    ((null? second) first)
    ((= which 1)
        first (cons (car first) (interleaves 2 (cdr first) second)))
    ((= which 2)
        second (cons (car second) (interleaves 1 first (cdr second))))
  )
)

(interleave (list 1 3 5) (list 2 4 6))
; expect (1 2 3 4 5 6)

(interleave (list 1 3 5) nil)
; expect (1 3 5)

(interleave (list 1 3 5) (list 2 4))
; expect (1 2 3 4 5)


(define (accumulate combiner start n term)
  (begin
    (define (acl combiner cur n term)
      (if (< cur n)
        (combiner (term cur) (acl combiner (+ cur 1) n term))
        (term cur)
      )
    )
    (combiner start (acl combiner 1 n term))
  )
)


(define (no-repeats lst)
  (cond
    ((null? lst) nil)
    (else (cons (car lst) (no-repeats 
      (filter-lst 
        (lambda (x) (not (= x (car lst))))
        (cdr lst)))))
  )
)
