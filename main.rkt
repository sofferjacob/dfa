#lang racket

(define (sign? c) (member c '(#\+ #\-)))
(define (operator? c) (member c '(#\+ #\- #\* #\/)))

(define (err) ('err))

(define (start c) (
    if (sign? c) [values n_sign #t "signo"] (if (char-numeric? c) (values int #t "entero")  (error (string-append [string c] " Error")))
))

(define (n_sign c) 
    (if (char-numeric? c) (values int #t "entero") (error (string-append [string c] " Error")))
)

(define (int c) (if (char-numeric? c) (values int #f "entero") (if (operator? c) [values op #t "operator"] (error (string-append [string c] " Error")))))

(define (op c) (if (char-numeric? c) [values int #t "entero"] (if (operator? c) [values n_sign #t "signo"] [error (string-append [string c] " Error")])))

(define accepted-states '(int))

(define tokenize (input) (
    let loop
        ([state start] [chars input] [res '()] [buffer ""] [curr-state "start"] [result ""])
        (if [empty? chars] (
            if (member state accepted-states) result #t result #f
        ) 
            (let-values (
                [(next trans strstate) (state (car chars))]
            ) 
                (if trans (
                    (loop next (cdr chars) )
                ))
            )
            )
        )
) )
