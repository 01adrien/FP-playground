#lang plait

(require "lib/core.rkt"
         "lib/combinators.rkt"
         "lib/parsers.rkt")

(print-only-errors #true)
; E ::= T + E | T
; T ::= F * T | F
; F ::= (E) | digit

(E : (Parser Number))
(define E 
  (lazy-parser 
    (lambda () 
      (Or 
        (Map3 (lambda (a _ b) (+ a b)) T (pchar #\+) E) T))))

(T : (Parser Number))
(define T 
  (lazy-parser 
    (lambda () 
      (Or 
        (Map3 (lambda (a _ b) (* a b)) F (pchar #\*) T) F))))

(F : (Parser Number))
(define F 
  (lazy-parser 
    (lambda () 
      (Or 
        (Map3 (lambda (_a b _c) b) (pchar #\() E (pchar #\))) pint))))


;; ---------- F ----------
(test (success-val (F "7")) 7)
(test (success-val (F "(7)")) 7)
(test (success-val (F "((7))")) 7)
(test (success-val (F "(1+2)")) 3)
(test (success-rem (F "7+1")) "+1")
(test (failure? (F "")) #t)
(test (failure? (F "a")) #t)
(test (failure? (F "(7")) #t) ;; parenthèse jamais fermée
(test (failure? (F "()")) #t) ;; rien dedans

;; ---------- T ----------
(test (success-val (T "4")) 4)
(test (success-val (T "2*3")) 6)
(test (success-val (T "2*3*4")) 24)
(test (success-val (T "(1+1)*3")) 6)
(test (success-rem (T "2*3+1")) "+1") ;; T ne mange pas le +
(test (failure? (T "*3")) #t)

;; ---------- E ----------
(test (success-val (E "1")) 1)
(test (success-val (E "1+2")) 3)
(test (success-val (E "1+2+3")) 6)
(test (success-val (E "2*3")) 6)
(test (success-val (E "1+2*3")) 7) ;; * plus prioritaire que +
(test (success-val (E "2*3+4")) 10)
(test (success-val (E "(1+2)*3")) 9)
(test (success-val (E "2*(3+4)")) 14)
(test (success-val (E "((5))")) 5)
(test (success-rem (E "1+2*3")) "")

;; échecs
(test (failure? (E "")) #t)
(test (failure? (E "+1")) #t)
(test (failure? (E "a")) #t)
(test (failure? (E "(1")) #t)

;; limite connue : "1+" réussit avec 1 et laisse "+" ; il faudrait eof
(test (success-val (E "1+")) 1)
(test (success-rem (E "1+")) "+")
