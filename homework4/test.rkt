#lang racket
(provide (all-defined-out))

(define sum
    (lambda (xs)
    (if (null? xs)
        0
        (+ car (xs) (sum (cdr xs))))))

(define (m-append xs ys)
    (if (null? xs)
        ys
        (cons (car xs) (append (cdr xs) ys))))

(define (m-map f xs)
    (if (null? xs)
        null
        (cons (f (car xs)) (m-map f (cdr xs)))))

(define powers-of-two
    (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
        (lambda () (f 2)))) ; return a function, which contains(when it's called) a pair
                            ;'(value, promise(the uncalled function containing the next pair))

(define (number-until stream tester)
    (letrec ([f (lambda (stream ans)
        (let ([pr (stream)])    ; the pair returned from calling function 'stream'
            (if (tester (car pr))   ; the value of the stream (stored at the pair 'pr')
                ans
                (f (cdr pr) (+ ans 1)))))])
    (f stream 1)))  ; tail recursion
