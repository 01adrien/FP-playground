#lang plait

(require "lib/core.rkt"
         "lib/combinators.rkt"
         "lib/parsers.rkt")

(print-only-errors #true)
; E ::= T + E | T
; T ::= F * T | F
; F ::= (E) | digit

(E : (Parser Number))
(define E (lazy-parser (lambda () (Or (doPlus (Triple T (pchar #\+) E)) T))))

(T : (Parser Number))
(define T (lazy-parser (lambda () (Or (doTimes (Triple F (pchar #\*) T)) F))))

(F : (Parser Number))
(define F (lazy-parser (lambda () (Or pnum (Map triple-snd (Triple (pchar #\() E (pchar #\))))))))

(triple-snd : (('a * ('b * 'c)) -> 'b))
(define (triple-snd v)
  (fst (snd v)))
;
;
(doTimes : ((Parser (Number * (Char * Number))) -> (Parser Number)))
(define (doTimes p)
  (Map (lambda (v) (* (fst v) (snd (snd v)))) p))

(doPlus : ((Parser (Number * (Char * Number))) -> (Parser Number)))
(define (doPlus p)
  (Map (lambda (v) (+ (fst v) (snd (snd v)))) p))

;; ---------- F ----------
(test (success-value (F "7")) 7)
(test (success-value (F "(7)")) 7)
(test (success-value (F "((7))")) 7)
(test (success-value (F "(1+2)")) 3)
(test (success-remaining (F "7+1")) "+1")
(test (failure? (F "")) #t)
(test (failure? (F "a")) #t)
(test (failure? (F "(7")) #t) ;; parenthèse jamais fermée
(test (failure? (F "()")) #t) ;; rien dedans

;; ---------- T ----------
(test (success-value (T "4")) 4)
(test (success-value (T "2*3")) 6)
(test (success-value (T "2*3*4")) 24)
(test (success-value (T "(1+1)*3")) 6)
(test (success-remaining (T "2*3+1")) "+1") ;; T ne mange pas le +
(test (failure? (T "*3")) #t)

;; ---------- E ----------
(test (success-value (E "1")) 1)
(test (success-value (E "1+2")) 3)
(test (success-value (E "1+2+3")) 6)
(test (success-value (E "2*3")) 6)
(test (success-value (E "1+2*3")) 7) ;; * plus prioritaire que +
(test (success-value (E "2*3+4")) 10)
(test (success-value (E "(1+2)*3")) 9)
(test (success-value (E "2*(3+4)")) 14)
(test (success-value (E "((5))")) 5)
(test (success-remaining (E "1+2*3")) "")

;; échecs
(test (failure? (E "")) #t)
(test (failure? (E "+1")) #t)
(test (failure? (E "a")) #t)
(test (failure? (E "(1")) #t)

;; limite connue : "1+" réussit avec 1 et laisse "+" ; il faudrait eof
(test (success-value (E "1+")) 1)
(test (success-remaining (E "1+")) "+")
