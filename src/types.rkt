#lang plait

(define-type-alias Env (Hashof Symbol Value))

(define-type BinOp [plus] [mult] [div] [minus])

(define-type Type [numT] [strT] [nulT] [boolT])

(define-type Exp
             [numE (n : Number)]
             [boolE (b : Boolean)]
             [binE (op : BinOp) (left : Exp) (right : Exp)]
             [cndE (test : Exp) (then : Exp) (else : Exp)]
             [varE (name : Symbol)]
             [letE (bindings : (Listof (Symbol * Exp))) (body : Exp)]
             [lamE (params : (Listof Symbol)) (body : Exp)]
             [appE (fun : Exp) (args : (Listof Exp))])

(define-type Value
             [numV (n : Number)]
             [boolV (b : Boolean)]
             [lamV (params : (Listof Symbol)) (body : Exp) (env : Env)])

(char->op : (Char -> BinOp))
(define (char->op c)
  (cond
    [(char=? c #\-) (minus)]
    [(char=? c #\+) (plus)]
    [(char=? c #\*) (mult)]
    [(char=? c #\/) (div)]))

(op->string : (BinOp -> String))
(define (op->string op)
  (type-case BinOp op [(mult) "*"] [(plus) "+"] [(div) "/"] [(minus) "-"]))
