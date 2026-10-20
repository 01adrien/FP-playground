#lang plait

(require "types.rkt"
         "parser.rkt")
(print-only-errors #true)

(define env (hash empty)) ;; "empty environment"

;; ---------------------
;; ---- Interpreter ----
;; ---------------------

(define (interp e nv)
  (type-case Exp
             e
             [(numE n) (numV n)]
             [(boolE b) (boolV b)]
             [(varE s) (lookup s nv)]
             [(plusE l r) (add (interp l nv) (interp r nv))]
             [(cndE c t e)
              (if (boolean-decision (interp c nv))
                  (interp t nv)
                  (interp e nv))]
             [(let1E var val body)
              (let ([new-env (extend nv var (interp val nv))]) (interp body new-env))]))

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

(define (lookup (s : Symbol) (n : Env))
  (type-case (Optionof Value) (hash-ref n s) [(none) (error s "not bound")] [(some v) v]))

(extend : (Env Symbol Value -> Env))
(define (extend old-env new-name value)
  (hash-set old-env new-name value))

(display (interp (parse "{let {[ok false]} {if ok true false}}") env))
