#lang plait

(require "types.rkt"
         "parser.rkt"
         "parser/lib/utils.rkt")

(require (typed-in racket/base
                   [char->integer : (Char -> Number)]
                   [integer->char : (Number -> Char)]
                   [number->string : (Number -> String)]))

(desugar : (Exp -> Exp))
(define (desugar exp)
  (type-case Exp
             exp
             [(numE val) exp]
             [(boolE val) exp]
             [(varE sym) exp]
             [(binE op left right) exp]
             [(letE bindings body)
              (letE (map (lambda (b) (pair (fst b) (desugar (snd b)))) bindings) (desugar body))]
             [(cndE _cond _if _else) exp]
             [(lamE params body) (foldr (lambda (curr acc) (lamE (list curr) acc)) body params)]
             [(appE fun args) (foldr (lambda (curr acc) (appE acc (list curr))) fun args )]))

(indent : (Number -> String))
(define (indent n)
  (if (<= n 0)
      ""
      (string-append " " (indent (- n 1)))))

(exp->string : (Exp Number -> String))
(define (exp->string exp i)
  (type-case
   Exp
   exp
   [(numE val) (number->string val)]
   [(boolE val)  (if (eq? val #t) "true" "false")]
   [(varE sym) (string-concat (list "<" (symbol->string sym) ">"))]
   [(binE op left right)
    (string-concat (list "(" (op->string op) " " (exp->string left i) " " (exp->string right i)  ")"))]
   [(letE bindings body)
    (string-concat (list "let "
                         (symbol->string (fst (first bindings))) "\n"
                         (indent (+ i 1)) (exp->string (snd (first bindings)) (+ i 2))
                         (exp->string body i)))]
   [(cndE _cond _if _else) "TODO"]
   [(lamE params body)
    (string-concat (list "lam " (symbol->string (first params)) "\n" (indent i) (exp->string body (+ i 1))))]
   [(appE fun args) (string-concat (list "(" (exp->string fun i) " " (exp->string (first args) i) ")"))]))
