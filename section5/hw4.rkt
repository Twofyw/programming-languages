
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below


; Problem 1
(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      empty))

; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (list-ref xs (remainder n (length xs)))]))

; Problem 4
(define (stream-for-n-steps s n)
  (if (> n 0)
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))
      empty))

; Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                               (cons (- x) (lambda () (f (+ x 1))))
                               (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

; Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x) (if x
                              (cons "dan.jpg" (lambda () (f (not x))))
                              (cons "dog.jpg" (lambda () (f (not x))))))])
    (lambda () (f #t))))

; Problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (s) (if (empty? s)
                              s
                              (cons (cons 0 (car (s))) (lambda ()(f (cdr (s)))))))])
    (lambda () (f s))))

; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

; Problem 9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [element (lambda (n) (if (>= n len) #f
                                    (let ([vc (vector-ref vec n)])
                                      (if (pair? vc)
                                          (if (equal? v (car vc))
                                              vc
                                              (element (+ n 1)))
                                          (element (+ n 1)))
                                      )))])
  (element 0)))

; Problem 10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector (length xs))]
           [slot 0]
           [f (lambda (v)
                (let ([search (vector-assoc v memo)])
                  (if search
                      search
                      (let ([run (assoc n xs)])
                        (if run
                            (begin (vector-set! memo slot run)
                                   (set! slot (add1 slot))
                                   run)
                            #f)))))])
    f))

; Challenge Problem
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([x1 e1]
              [x2 e2]
              [while (lambda (x2) (if (< x2 x1)
                                      (while e2)
                                      #t))])
       (while e2))]))
