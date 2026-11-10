#lang plait

(require "utils.rkt"
         "core.rkt"
         "combinators.rkt"
         "parsers.rkt")

(require (typed-in racket/base
[char->integer : (Char -> Number)]
[integer->char : (Number -> Char)]
[number->string : (Number -> String)]))


;; ---------------
;; PEG operateurs
;; ---------------

(define-type Grammar
             [eps] ; ε, la chaîne vide
             [lit (text : String)] ; 'abc' ou "abc"
             [ref (name : Symbol)] ; un non-terminal
             [seq (parts : (Listof Grammar))] ; e1 e2 e3
             [alt (choices : (Listof Grammar))] ; e1 / e2 / e3
             [star (g : Grammar)] ; e*
             [not-p (g : Grammar)]) ; !e

(define-type-alias Rule (Symbol * Grammar))
(define-type-alias Lang (Listof Rule))

;; (and-p g) -> &e
(and-p : (Grammar -> Grammar))
(define (and-p g)
  (not-p (not-p g)))

;; (one-of s) -> [...]
(one-of : (String -> Grammar))
(define (one-of s)
  (alt (map (lambda (e) (lit (char->string e))) (string->list s))))

;; (opt g) -> e?
(opt : (Grammar -> Grammar))
(define (opt g)
  (alt (list g (eps))))

;; (plus g) -> e+
(plus : (Grammar -> Grammar))
(define (plus g)
  (seq (list g (star g))))
