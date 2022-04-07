#lang racket

; TODO: Add spaces

(define (sign? c) (member c '(#\+ #\-)))
(define (operator? c) (member c '(#\+ #\- #\* #\/)))

(define (err) ('err))

(define (start  c) 
    "Estado inicial"
(
    cond
        [(sign? c) [values n_sign #t "signo"]]
        [(char-numeric? c) [values int #t "entero"]]
        [(eq? c #\() [values o_par #t "parentesis abierto"]]
        [else (error (string-append [string c] " Error"))]
))

; (define (start c) (
;     if (sign? c) [values n_sign #t "signo"] (if (char-numeric? c) (values int #t "entero")  (error (string-append [string c] " Error")))
; ))

(define (n_sign c) 
    (if (char-numeric? c) (values int #t "entero") (error (string-append [string c] " Error")))
)

(define (o_par c) (
    if (char-numeric? c) (values int #t "entero") (if (sign? c) [values n_sign #t "signo"] [error (string-append [string c] " Error")])
))

(define (int c) (if (char-numeric? c) (values int #f "entero") (if (operator? c) [values op #t "operator"] (error (string-append [string c] " Error")))))

(define (op c) (if (char-numeric? c) [values int #t "entero"] (if (operator? c) [values n_sign #t "signo"] [error (string-append [string c] " Error")])))

(define accepted-states '(int))

(define (dfa input) (tokenize (string->list input)))

(define (tokenize input) (
    let loop
        ([state start] [chars input] [res "tipo | token"] [buffer ""] [curr-state "start"])
        (if [empty? chars] (
            if (> [string-length buffer] 0) 
                (loop state chars [string-append res buffer] "" curr-state)
                (if (member state accepted-states) res res)
            ) 
            (let-values (
                [(next trans strstate) (state (car chars))]
            ) 
                (if trans
                    (loop next (cdr chars) 
                        (if [> (string-length buffer) 0]
                            (string-append res buffer " \n" strstate " " [string(car chars)] " \n")
                            ;(cons (string-append strstate " " (string[car chars])) (cons [string-append curr-state " " buffer] res))
                            (string-append res " \n" strstate " " [string(car chars)])
                            ;(cons [string-append strstate " " (string[car chars])] res)
                        ) "" strstate
                    )
                    (loop next (cdr chars) res (string-append buffer [string(car chars)]) strstate)
                )
            )
        )
) )
