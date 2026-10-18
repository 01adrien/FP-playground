#lang plait

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

; Interpreter
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

(test (interpret (numE 1)) (numV 1))
(test (interpret (numE 2.3)) (numV 2.3))
(test (interpret (plusE (numE 1) (numE 2))) (numV 3))
(test (interpret (plusE (plusE (numE 1) (numE 2)) (numE 3))) (numV 6))
(test (interpret (plusE (numE 1) (plusE (numE 2) (numE 3)))) (numV 6))
(test (interpret (plusE (numE 1) (plusE (plusE (numE 2) (numE 3)) (numE 4)))) (numV 10))

; Parser
(parse : (S-Exp -> Exp))
(define (parse s)
  (cond
    [(s-exp-number? s) (numE (s-exp->number s))]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (if (symbol=? '+ (s-exp->symbol (first l)))
           (plusE (parse (second l)) (parse (third l)))
           (error 'parse "list not an addition")))]))

(test (parse `1) (numE 1))
(test (parse `2.3) (numE 2.3))
(test (parse `{+ 1 2}) (plusE (numE 1) (numE 2)))
(test (parse `{+ 1 {+ {+ 2 3} 4}}) (plusE (numE 1) (plusE (plusE (numE 2) (numE 3)) (numE 4))))

; Interpreter
(run : (S-Exp -> Value))
(define (run s)
  (interpret (parse s)))

(test (run `2.3) (numV 2.3))
(test (run `{+ 1 2}) (numV 3))
(test (run `{+ {+ 1 2} 3}) (numV 6))
(test (run `{+ 1 {+ 2 3}}) (numV 6))
(test (run `{+ 1 {+ {+ 2 3} 4}}) (numV 10))
