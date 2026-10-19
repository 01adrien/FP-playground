#lang plait

(require "parser/lib/core.rkt"
         "parser/lib/combinators.rkt"
         "parser/lib/parsers.rkt")

; GRAMMAR
; <expr> ::= <num>
;          | {+ <expr> <expr>}
;          | {let1 {<var> <expr>} <expr>}
;          | <var>

(print-only-errors #true)

(define-type-alias Env (Hashof Symbol Value))
(define mt-env (hash empty)) ;; "empty environment"

(define-type Exp
             [numE (n : Number)]
             [boolE (b : Boolean)]
             [plusE (left : Exp) (right : Exp)]
             [cndE (test : Exp) (then : Exp) (else : Exp)]
             [varE (name : Symbol)]
             [let1E (var : Symbol) (value : Exp) (body : Exp)])

(define-type Value [numV (n : Number)] [boolV (b : Boolean)])

;; --------
;; Parser
;; --------

(numP : (Parser Exp))
(define numP (Map numE pint))

(plusP : ((Char * (Exp * Exp)) -> Exp))
(define (plusP v)
  (plusE (fst (snd v)) (snd (snd v))))

(expP : (Parser Exp))
(define expP (lazy-parser (lambda () (Between (Many pspace) (Or numP opP) (Many pspace)))))

(addP : (Parser Exp))
(define addP (Map plusP (Triple (And (Skip (Many pspace)) (pchar #\+)) expP expP)))

(opP : (Parser Exp))
(define opP (Between (pchar #\{) addP (pchar #\})))

(display ((Many expP) "{+ 555 {+ -6 {+ 789 -8}}}"))

;; ------------
;; Interpreter
;; ------------
(interpret : (Exp -> Value))
(define (interpret e)
  (type-case Exp
             e
             [(numE n) (numV n)]
             [(boolE b) (boolV b)]
             [(plusE l r) (add (interpret l) (interpret r))]
             [(cndE c t e)
              (if (eq? (boolean-decision (interpret c)) #t)
                  (interpret t)
                  (interpret e))]
             [(let1E s v b) (numV 0)]
             [(varE s) (numV 0)]))

(add : (Value Value -> Value))
(define (add v1 v2)
  (type-case
   Value
   v1
   [(numV n1)
    (type-case Value v2 [(numV n2) (numV (+ n1 n2))] [else (error '+ "expects RHS to be a number")])]
   [else (error '+ "expects LHS to be a number")]))

(boolean-decision : (Value -> Boolean))
(define (boolean-decision v)
  (type-case Value v [(numV n) (> 0 n)] [(boolV b) b]))

(test (interpret (numE 1)) (numV 1))
(test (interpret (numE 2.3)) (numV 2.3))
(test (interpret (plusE (numE 1) (numE 2))) (numV 3))
(test (interpret (plusE (plusE (numE 1) (numE 2)) (numE 3))) (numV 6))
(test (interpret (plusE (numE 1) (plusE (numE 2) (numE 3)))) (numV 6))
(test (interpret (plusE (numE 1) (plusE (plusE (numE 2) (numE 3)) (numE 4)))) (numV 10))
