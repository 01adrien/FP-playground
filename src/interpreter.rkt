#lang plait

(require "types.rkt"
         "parser.rkt"
         "desugar.rkt")

(define the-env (hash empty))

(interp : (Exp Env -> Value))
(define (interp exp env)
  (type-case Exp
             exp
             [(numE val) (numV val)]
             [(boolE val) (boolV val)]
             [(varE sym) (lookup sym env)]
             [(binE op left right) (math-op op (interp left env) (interp right env))]
             [(letE bindings body) (interp body (foldl extend-list env bindings))]
             [(cndE _cond _if _else)
              (if (boolean-decision (interp _cond env))
                  (interp _if env)
                  (interp _else env))]
             [(lamE params body) (lamV params body env)]
             [(appE fun args) (interp-app fun args env)]))

(op->symbol : (BinOp -> Symbol))
(define (op->symbol op)
  (type-case BinOp op [(plus) '+] [(minus) '-] [(mult) '*] [(div) '/]))

(math-op : (BinOp Value Value -> Value))
(define (math-op op v1 v2)
  (type-case Value
             v1
             [(numV n1)
              (type-case Value
                         v2
                         [(numV n2)
                          (type-case BinOp
                                     op
                                     [(plus) (numV (+ n1 n2))]
                                     [(minus) (numV (- n1 n2))]
                                     [(mult) (numV (* n1 n2))]
                                     [(div)
                                      (if (= n2 0)
                                          (error '/ "division by zero")
                                          (numV (/ n1 n2)))])]
                         [else (error (op->symbol op) "expects RHS to be a number")])]
             [else (error (op->symbol op) "expects LHS to be a number")]))

(extend-list : ((Symbol * Exp) Env -> Env))
(define (extend-list binding env)
  (extend env (fst binding) (interp (snd binding) env)))

(extend : (Env Symbol Value -> Env))
(define (extend old-env new-name value)
  (hash-set old-env new-name value))

(interp-app : (Exp (Listof Exp) Env -> Value))
(define (interp-app fun args env)
  (let ([fname (varE-name fun)])
    (type-case Value
               (interp fun env)
               [(lamV params body e)
                (interp body (foldl extend-list e (eval-args params args fname)))]
               [else (error fname "not a function")])))

(eval-args : ((Listof 'a) (Listof 'b) Symbol -> (Listof ('a * 'b))))
(define (eval-args lst1 lst2 fn)
  (cond
    [(not (eq? (length lst1) (length lst2))) (error fn "invalid args count")]
    [(empty? lst1) empty]
    [else (cons (pair (first lst1) (first lst2)) (eval-args (rest lst1) (rest lst2) fn))]))

(boolean-decision : (Value -> Boolean))
(define (boolean-decision v)
  (type-case Value v [(numV n) (> n 0)] [(boolV b) b] [else #f]))

(define (lookup (s : Symbol) (n : Env))
  (type-case (Optionof Value) (hash-ref n s) [(none) (error s "not bound")] [(some v) v]))

#|
(interp (parse "{let {[x 1]} {let {[f {lam y {+ 5 x}}]} {let {[x 78]} {f 10}}}}") the-env)
(interp (parse "{let {[f {lam y {+ x 5}}]} {let {[x 78]} {f 10}}}") the-env)
 |#

(let ([res (desugar (parse "{let {[fun {lam a b c {+ a {+ b c}}}]} {fun 5 10 15}}"))])
  (begin
    (display res)
    (display "\n\n")
    (display (exp->string res 0))))

;; (interp (parse "{let {[f {lam y {+ x 5}}]} {let {[x 78]} {f 10}}}") the-env)

(print-only-errors #true)

;; =====================================================================
;; TESTS INTERP
;; =====================================================================

(define (run s)
  (interp (parse s) (hash empty)))

;; ---------- littéraux ----------
(test (run "5") (numV 5))
(test (run "-6") (numV -6))
(test (run "true") (boolV #t))
(test (run "false") (boolV #f))

;; ---------- opérations binaires ----------
(test (run "{+ 1 2}") (numV 3))
(test (run "{- 5 3}") (numV 2))
(test (run "{- 3 5}") (numV -2))
(test (run "{* 4 3}") (numV 12))
(test (run "{/ 6 3}") (numV 2))
(test (run "{+ -6 5}") (numV -1))
(test (run "{* -2 -3}") (numV 6))
(test (run "{+ 1 {* 2 {- 8 {/ 6 3}}}}") (numV 13))
(test (run "{- 10 {- 5 {- 3 1}}}") (numV 7)) ; 10 - (5 - 2)
(test (run "{- {- 10 5} 3}") (numV 2))
(test (run "{+ {+ 1 2} {+ 3 4}}") (numV 10))

;; erreurs de type et de division
(test/exn (run "{+ true 1}") "LHS")
(test/exn (run "{+ 1 true}") "RHS")
(test/exn (run "{- true 1}") "LHS")
(test/exn (run "{* 1 false}") "RHS")
(test/exn (run "{/ 1 true}") "RHS")
(test/exn (run "{/ 1 0}") "zero")
(test/exn (run "{/ 5 {- 2 2}}") "zero")

;; ---------- if ----------
(test (run "{if true 1 2}") (numV 1))
(test (run "{if false 1 2}") (numV 2))
(test (run "{if {if true false true} 1 2}") (numV 2))
(test (run "{if true {+ 1 2} {+ 3 4}}") (numV 3))
(test (run "{if false {+ 1 2} {+ 3 4}}") (numV 7))
(test (run "{+ {if true 1 2} {if false 3 4}}") (numV 5))
;; une seule branche est évaluée (l'autre contiendrait une erreur)
(test (run "{if true 1 {/ 1 0}}") (numV 1))
(test (run "{if false {/ 1 0} 2}") (numV 2))

;; ---------- let ----------
(test (run "{let {[x 5]} x}") (numV 5))
(test (run "{let {[x 5]} {+ x 1}}") (numV 6))
(test (run "{let {[x 1] [y 2]} {+ x y}}") (numV 3))
(test (run "{let {[a 1] [b 2] [c 3]} {+ a {+ b c}}}") (numV 6))
;; (test (run "{let {[x 1]} {let {[y 2]} {+ x y}}}") (numV 3))
(test (run "{let {[a 1]} {let {[b {+ a 1}]} {+ a b}}}") (numV 3))
(test (run "{let {[x {let {[y 1]} y}]} x}") (numV 1))
(test (run "{let {[b true]} b}") (boolV #t))
(test (run "{let {[ok {+ 5 6}]} {if ok true false}}") (boolV #t)) ; ok = 11 : dépend de ta spec
(test (run "{let {[x 5]} {if true x 0}}") (numV 5))

;; masquage
(test (run "{let {[x 1]} {let {[x 2]} x}}") (numV 2))
(test (run "{let {[x 1]} {+ {let {[x 2]} x} x}}") (numV 3)) ; le x interne ne fuit pas
(test (run "{let {[x 1]} {let {[x {+ x 1}]} x}}") (numV 2))

;; variables libres
(test/exn (run "x") "")
(test/exn (run "{+ x 1}") "")
(test/exn (run "{let {[x 1]} y}") "")
(test/exn (run "{let {[x 1]} {+ x z}}") "")
;; la portée du let ne dépasse pas son corps
(test/exn (run "{+ {let {[x 1]} x} x}") "")

;; à décider selon ta spec (liaisons parallèles ou séquentielles ?) :
;; (test/exn (run "{let {[x 1] [y x]} y}") "")  ; parallèle : x inconnu
;; (test (run "{let {[x 1] [y x]} y}") (numV 1)) ; séquentiel
;; (test/exn (run "{let {[x 1] [x 2]} x}") "")  ; doublon refusé ?

;; ---------- lam ----------
(test (type-case Value (run "{lam x {+ x 1}}") [(lamV p b e) #t] [else #f]) #t)

;; ---------- application ----------
;; NB : avec ton parseur, `{lam x x}` ne passe pas (Many1 pid avale aussi le corps),
;; donc les corps ci-dessous ne commencent pas par un identifiant.
(test (run "{let {[f {lam x {+ x 1}}]} {f 5}}") (numV 6))
(test (run "{let {[f {lam x {* x x}}]} {f 4}}") (numV 16))
(test (run "{let {[f {lam x y {+ x y}}]} {f 1 2}}") (numV 3))
(test (run "{let {[f {lam x y {- x y}}]} {f 10 3}}") (numV 7))
;; (test (run "{let {[f {lam x {+ x 1}}]} {f {f 1}}}") (numV 3))
(test (run "{let {[f {lam x {+ x 1}}]} {+ {f 1} {f 2}}}") (numV 5))
(test (run "{let {[f {lam x {if x 1 2}}]} {f true}}") (numV 1))
(test (run "{let {[f {lam x {+ x 0}}]} {f {+ 1 2}}}") (numV 3)) ; argument évalué avant l'appel

;; ---------- portée lexicale / fermetures ----------
;; x capturé à la définition de f, pas à l'appel
(test (run "{let {[x 1]} {let {[f {lam y {+ x y}}]} {let {[x 100]} {f 5}}}}") (numV 6))
;; le paramètre masque la variable extérieure
(test (run "{let {[x 1]} {let {[f {lam x {+ x 0}}]} {f 7}}}") (numV 7))
;; la fermeture garde son environnement
(test (run "{let {[mk {lam a {lam b {+ a b}}}]} {let {[add5 {mk 5}]} {add5 3}}}") (numV 8))
;; ton exemple : x est libre dans f à la définition -> erreur
(test/exn (run "{let {[f {lam y {+ x 5}}]} {let {[x 78]} {f 10}}}") "x: not bound")

;; ---------- erreurs d'application ----------
(test/exn (run "{let {[f 5]} {f 1}}") "not a function")
(test/exn (run "{let {[f true]} {f 1}}") "not a function")
(test/exn (run "{g 1}") "") ; fonction inconnue
;; arité incorrecte (si eval-args la vérifie)
(test/exn (run "{let {[f {lam x y {+ x y}}]} {f 1}}") "args count")
(test/exn (run "{let {[f {lam x {+ x 1}}]} {f 1 2}}") "args count")
;; erreur dans un argument
(test/exn (run "{let {[f {lam x {+ x 1}}]} {f {/ 1 0}}}") "zero")
;; erreur dans le corps
(test/exn (run "{let {[f {lam x {+ x true}}]} {f 1}}") "RHS")
