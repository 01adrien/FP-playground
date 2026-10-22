#lang plait

(require "lib/combinators.rkt"
         "lib/parsers.rkt")

(define-type Grammar
             [eps] ; ε, la chaîne vide
             [lit (text : String)] ; 'abc' ou "abc"
             [ref (name : Symbol)] ; un non-terminal
             [seq (parts : (Listof Grammar))] ; e1 e2 e3
             [alt (choices : (Listof Grammar))] ; e1 / e2 / e3
             [star (e : Grammar)] ; e*
             [not-p (e : Grammar)]) ; !e

(define-type-alias Rule (Symbol * Grammar))




(and-p : (Grammar -> Grammar))
(define (and-p g) (not-p (not-p g)))

; [one-of (allowed : String)] classe de caractères [abc]
(one-of (String -> Grammar))
(define )


; [opt (e : Grammar)] ; e?
;  [plus (e : Grammar)] ; e+