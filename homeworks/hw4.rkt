;#lang racket

;(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1.
(define (sequence low high stride) =
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;; 2.
(define (string-append-map xs suffix) =
  (map  (lambda (x) = (string-append x suffix)) xs))


;; 3.
(define (list-nth-mod ys k) 
  (define (list-nth xs n)
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(null? xs) (error  "list-nth-mod: empty list")]
          [(= n 0) (car xs)]
          [#t (list-nth (cdr xs) (- n 1))]))
  (define mod-n (remainder k (length ys)))
  (list-nth ys mod-n))


;; 4.
(define (stream-for-n-steps s n) 
   (cond [(< n 0) (error "stream-for-n-steps: negative number")]
         [(= n 0) '()]
         [#t (cons (car (s))  (stream-for-n-steps (cdr (s)) (- n 1)))]))


;; 5.
(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([m (remainder x 5)])
                  (if (= m 0)
                      (cons (- x) (lambda () (f (+ x 1))))
                      (cons x (lambda () (f (+ x 1)))))
                  )
                )])
           (lambda ()(f 1))))

(define (stream-nth s n)
  (cond [(< n 0) (error "stream-nth: negative number")]
        [(= n 1) (car (s))]
        [#t (stream-nth (cdr (s)) (- n 1))]))
  

;; 6.
(define dan-then-dog
  (lambda ()
    (cons "dan.jpg" 
          (lambda ()
            (cons "dog.jpg" dan-then-dog)))))
  


;; 7.

;; aux stream for testing
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; aux stream for testing
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(define (stream-add-zero s)
  (lambda () 
    (cons (cons 0 (car (s))) 
          (stream-add-zero (cdr (s)) ))))


;; 8.

(define (cycle-lists xs1 ys1)
  (letrec ([list-to-stream 
            (lambda (xs xs-origin)
              (cond [(null? xs) (list-to-stream xs-origin xs-origin)]
                    [#t (cons (car xs) 
                              (lambda () (list-to-stream (cdr xs) xs-origin)))]
                    )
              )])
    (begin (define s1 (lambda () (list-to-stream xs1 xs1)))
           (define s2 (lambda () (list-to-stream ys1 ys1)))
           (define (f a b)  (cons (cons (car (a)) (car (b)))
                                            (lambda () (f (cdr (a)) (cdr (b)))))
                                   )
            (lambda () (f s1 s2))
           )
    )
  )

;; 9.

(define (vector-assoc v vec)
  (let* ([pred (lambda (p) (if (pair? p)
                              (= (car p) v)
                              #f))]
        [pred-curry (lambda (a)
                      (lambda (p)
                        (if (pair? p)
                              (equal? (car p) a)
                              #f)))]
        [fs (vector-filter (pred-curry v) vec)]
        [n (vector-length fs)]
        )
    (if (> n 0)
        (vector-ref fs 0)
        #f)
    )
)







