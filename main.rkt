#lang racket

(require racket/trace)

(define (sign? c) (member c '(#\+ #\-)))
(define (operator? c) (member c '(#\+ #\- #\* #\/)))

(define (err) ('err))

(define (start  c) 
    "Estado inicial"
(
    cond
        [(sign? c) [values n_sign #f "signo" "start"]]
        [(char-numeric? c) [values int #f "entero" "start"]]
        [(char-alphabetic? c) [values var #f "variable" "start"]]
        [(eq? c #\space) [values start #f "espacio" "start"]]
        [(eq? c #\() [values o_par #f "parentesis abierto" "start"]]
        [else (error (string-append [string c] " Error"))]
))

(define (n_sign c)
    "Estado de signo de número"
    (
    cond
        [(char-numeric? c) [values int #t "entero" "signo"]]
        [(eq? c #\space) [values n_sign #f "signo" "signo"]]
        [(eq? c #\() [values o_par #t "parentesis abierto" "signo"]]
        [else (error (string-append [string c] " Error"))]
)
)

(define (o_par c)
    "Estado de parentesis abierto"
    (
    cond
        [(sign? c) [values n_sign #t "signo" "parentesis abierto"]]
        [(char-numeric? c) [values int #t "entero" "parentesis abierto"]]
        [(eq? c #\space) [values o_par #f "parentesis abierto" "parentesis abierto"]]
        [(char-alphabetic? c) [values var #t "variable" "parentesis abierto"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (int c)
    "Estado número entero"
    (
    cond
        [(operator? c) [values op #t "operator" "entero"]]
        [(char-numeric? c) [values int #f "entero" "entero"]]
        [(eq? c #\.) [values real #f "real" "real"]]
        [(eq? c #\() [values o_par #t "parentesis abierto" "entero"]]
        [(eq? c #\)) [values c_par #t "parentesis cerrado" "entero"]]
        [(eq? c #\space) [values int #f "entero" "entero"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (op c)
    "Estado operador"
    (
    cond
        [(sign? c) [values n_sign #t "signo" "operador"]]
        [(char-numeric? c) [values int #t "entero" "operador"]]
        [(eq? c #\space) [values op #f "operador" "operador"]]
        [(eq? c #\() [values o_par #t "parentesis abierto" "operador"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (var c)
    "Estado variable"
    (
    cond
        [(operator? c) [values op #t "operador" "variable"]]
        [(char-numeric? c) [values var #f "variable" "variable"]]
        [(char-alphabetic? c) [values var #f "variable" "variable"]]
        [(eq? c #\_) [values var #f "variable" "variable"]]
        [(eq? c #\=) [values assign #t "asignacion" "variable"]]
        [(eq? c #\space) [values var #f "variable" "variable"]]
        [(eq? c #\() [values o_par #t "parentesis abierto" "variable"]]
        [(eq? c #\)) [values c_par #t "parentesis cerrado" "variable"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (assign c)
    "Estado de asignación"
    (
    cond
        [(sign? c) [values n_sign #t "signo" "asignacion"]]
        [(char-numeric? c) [values int #t "entero" "asignacion"]]
        [(char-alphabetic? c) [values var #t "variable" "asignacion"]]
        [(eq? c #\space) [values assign #f "asignacion" "asignacion"]]
        [(eq? c #\() [values o_par #t "parentesis abierto" "asignacion"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (c_par c)
    "Estado parentesis cerrado"
    (
    cond
        [(operator? c) [values op #t "operador" "parentesis cerrado"]]
        [(char-numeric? c) [values int #t "entero" "parentesis cerrado"]]
        [(char-alphabetic? c) [values var #f "variable" "parentesis cerrado"]]
        [(eq? c #\space) [values c_par #f "parentesis cerrado" "parentesis cerrado"]]
        [(eq? c #\() [values o_par #t "parentesis abierto" "parentesis cerrado"]]
        [(eq? c #\)) [values c_par #t "parentesis cerrado" "parentesis cerrado"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (real c)
    "Estado número real"
    (
    cond
        [(operator? c) [values op #t "operador" "real"]]
        [(char-numeric? c) [values real #f "real" "real"]]
        [(or (eq? c #\e) (eq? c #\E)) [values realexp #f "real" "real"]]
        [(eq? c #\)) [values c_par #t "parentesis cerrado" "real"]]
        [(eq? c #\space) [values real #f "real" "real"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (realexp c)
    "Estado número real, con extensión de exponente"
    (
    cond
        [(sign? c) [values exp_sign #f "real" "real"]]
        [(char-numeric? c) [values exp_dig #f "real" "real"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (exp_sign c)
    "Estado número real, con extensión de exponente con signo"
    (
    cond
        [(char-numeric? c) [values exp_dig #f "real" "real"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define (exp_dig c)
    "Estado número real, con extensión de exponente y digito"
    (
    cond
        [(operator? c) [values op #t "operador" "real"]]
        [(eq? c #\() [values o_par #t "parentesis abierto" "real"]]
        [(eq? c #\)) [values c_par #t "parentesis cerrado" "real"]]
        [else (error (string-append [string c] " Error"))]
)   
)

(define accepted-states (list int real exp_dig  c_par))

(define (dfa input) (tokenize (string->list input)))

; (define (tokenize input) (
;     let loop
;         ([state start] [chars input] [tokens null] [types null] [buffer ""] [curr-state "start"])
;         (if [empty? chars] (
;                 let res
;                     ([c (reverse (cons buffer tokens))] [t (reverse (cons curr-state types))] [r "tipo | token"])
;                     (
;                        if (empty? t) (string-append r "\n") (res (cdr c) (cdr t) (string-append r "\n" (car t) " " (car c)))
;                     )
;             ) 
;             (let-values (
;                 [(next trans strstate cstr) (state (car chars))]
;             )
;                (loop
;                     next
;                     (cdr chars)
;                     (if trans [cons (string-append buffer) tokens] tokens)
;                     (if trans [cons cstr types] types)
;                     (if trans (string [car chars]) [string-append buffer (string [car chars])])
;                     strstate
;                )
;             )
;         )
; ) )

(define (tokenize input) (
    let loop
        ([state start] [chars input] [tokens null] [types null] [buffer ""] [curr-state "start"])
        (if [empty? chars] (values
                [map (lambda (a b) (cons a b)) (reverse (cons curr-state types)) (reverse (cons buffer tokens))]
                [if (member state accepted-states) #t #f]
            ) 
            (let-values (
                [(next trans strstate cstr) (state (car chars))]
            )
               (loop
                    next
                    (cdr chars)
                    (if trans [cons (string-append buffer) tokens] tokens)
                    (if trans [cons cstr types] types)
                    (if trans (string [car chars]) [string-append buffer (string [car chars])])
                    strstate
               )
            )
        )
) )