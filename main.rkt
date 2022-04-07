#lang racket

(define (sign? c) (member c '(#\+ #\-)))
(define (operator? c) (member c '(#\+ #\- #\* #\/)))

(define (err) ('err))

(define (start  c) 
    "Estado inicial"
(
    cond
        [(sign? c) [values n_sign #t "signo"]]
        [(char-numeric? c) [values int #t "entero"]]
        [(char-alphabetic? c) [values var #t "variable"]]
        [(eq? c #\space) [values start #f "espacio"]]
        [(eq? c #\() [values o_par #t "parentesis abierto"]]
        [else (error (string-append [string c] " Error"))]
))

; (define (start c) (
;     if (sign? c) [values n_sign #t "signo"] (if (char-numeric? c) (values int #t "entero")  (error (string-append [string c] " Error")))
; ))

(define (n_sign c)
    "Estado de signo de número"
    ;(if (char-numeric? c) (values int #t "entero") (error (string-append [string c] " Error")))
    (
    cond
        [(char-numeric? c) [values int #t "entero"]]
        [(eq? c #\space) [values n_sign #f "signo"]]
        [(eq? c #\() [values o_par #t "parentesis abierto"]]
        [else (error (string-append [string c] " Error"))]
)
)

(define (o_par c)
    "Estado de parentesis abierto"
    ;if (char-numeric? c) (values int #t "entero") (if (sign? c) [values n_sign #t "signo"] [error (string-append [string c] " Error")])
    (
    cond
        [(sign? c) [values n_sign #t "signo"]]
        [(char-numeric? c) [values int #t "entero"]]
        [(eq? c #\space) [values o_par #f "parentesis abierto"]]
        [(char-alphabetic? c) [values var #t "variable"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (int c)
    "Estado número entero"
    ;if (char-numeric? c) (values int #t "entero") (if (sign? c) [values n_sign #t "signo"] [error (string-append [string c] " Error")])
    (
    cond
        [(operator? c) [values op #t "operator"]]
        [(char-numeric? c) [values int #f "entero"]]
        [(eq? c #\.) [values real #f "real"]]
        [(eq? c #\() [values o_par #t "parentesis abierto"]]
        [(eq? c #\)) [values c_par #t "parentesis cerrado"]]
        [else (error (string-append [string c] " Error"))]
)   
)

;(define (int c) (if (char-numeric? c) (values int #f "entero") (if (operator? c) [values op #t "operator"] (error (string-append [string c] " Error")))))

;(define (op c) (if (char-numeric? c) [values int #t "entero"] (if (operator? c) [values n_sign #t "signo"] [error (string-append [string c] " Error")])))

(define (op c)
    "Estado operador"
    (
    cond
        [(sign? c) [values n_sign #t "signo"]]
        [(char-numeric? c) [values int #t "entero"]]
        [(eq? c #\space) [values op #f "operador"]]
        [(eq? c #\() [values o_par #t "parentesis abierto"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (var c)
    "Estado variable"
    (
    cond
        [(operator? c) [values op #t "operador"]]
        [(char-numeric? c) [values var #f "variable"]]
        [(char-alphabetic? c) [values var #f "variable"]]
        [(eq? c #\_) [values var #f "variable"]]
        [(eq? c #\=) [values assign #t "asignacion"]]
        [(eq? c #\space) [values var #f "variable"]]
        [(eq? c #\() [values o_par #t "parentesis abierto"]]
        [(eq? c #\)) [values c_par #t "parentesis cerrado"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (assign c)
    "Estado de asignación"
    (
    cond
        [(sign? c) [values n_sign #t "signo"]]
        [(char-numeric? c) [values int #t "entero"]]
        [(char-alphabetic? c) [values var #t "variable"]]
        [(eq? c #\space) [values assign #f "asignacion"]]
        [(eq? c #\() [values o_par #t "parentesis abierto"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (c_par c)
    "Estado parentesis cerrado"
    (
    cond
        [(operator? c) [values op #t "operador"]]
        [(char-numeric? c) [values int #t "entero"]]
        [(char-alphabetic? c) [values var #f "variable"]]
        [(eq? c #\space) [values c_par #f "parentesis cerrado"]]
        [(eq? c #\() [values o_par #t "parentesis abierto"]]
        [(eq? c #\)) [values c_par #t "parentesis cerrado"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (real c)
    "Estado número real"
    (
    cond
        [(operator? c) [values op #t "operador"]]
        [(char-numeric? c) [values real #f "real"]]
        [(or (eq? c #\e) (eq? c #\E)) [values realexp #f "real"]]
        [(eq? c #\)) [values c_par #t "parentesis cerrado"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (realexp c)
    "Estado número real, con extensión de exponente"
    (
    cond
        [(sign? c) [values exp_sign #f "real"]]
        [(char-numeric? c) [values exp_dig #f "real"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (exp_sign c)
    "Estado número real, con extensión de exponente con signo"
    (
    cond
        [(char-numeric? c) [values exp_dig #f "real"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (exp_dig c)
    "Estado número real, con extensión de exponente y digito"
    (
    cond
        [(operator? c) [values op #t "operador"]]
        [(eq? c #\() [values o_par #t "parentesis abierto"]]
        [(eq? c #\)) [values c_par #t "parentesis cerrado"]]
        [else (error (string-append [string c] " Error"))]
)   
)

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
                            (string-append res " \n" strstate " " [string(car chars)])
                        ) "" strstate
                    )
                    (loop next (cdr chars) res (string-append buffer [string(car chars)]) strstate)
                )
            )
        )
) )
