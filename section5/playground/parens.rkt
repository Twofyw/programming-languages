#lang racket

(provide (all-defined-out))

(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))

; how to do tail recursion in racket?

