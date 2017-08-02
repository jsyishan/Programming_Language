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
        (lambda () (f 2)))) ; return a function(thunk), which contains(when it's called) a pair
                            ;'(value, promise(the uncalled function(thunk) containing the next pair))

(define (number-until stream tester)
    (letrec ([f (lambda (stream ans)
        (let ([pr (stream)])    ; the pair returned from calling thunk 'stream'
            (if (tester (car pr))   ; the value of the stream (stored at the pair 'pr')
                ans
                (f (cdr pr) (+ ans 1)))))])
    (f stream 1)))  ; tail recursion


(define (fibonacci x)
    (letrec ([f (lambda (acc1 acc2 y)
        (if (= y x)
        (+ acc1 acc2)
        (f (+ acc1 acc2) acc1 (+ y 1))))])
    (if (or (= x 1)(= x 2))
        1
        (f 1 1 3))))

(define fibonacci2
    (letrec([memo null]
            [f (lambda (x)
                (let ([ans (assoc x memo)])
                    (if ans
                        (cdr ans)
                        (let ([new-ans (if (or (= x 1) (= x 2))
                                        1
                                        (+ (f (- x 1))(f (- x 2))))])
                        (begin(set! memo (cons (cons x new-ans) memo))
                               new-ans)))))])
    f))
