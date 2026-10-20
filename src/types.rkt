#lang plait


(define-type-alias Env (Hashof Symbol Value))

(define-type Exp
             [numE (n : Number)]
             [boolE (b : Boolean)]
             [plusE (left : Exp) (right : Exp)]
             [cndE (test : Exp) (then : Exp) (else : Exp)]
             [varE (name : Symbol)]
             [let1E (var : Symbol) (value : Exp) (body : Exp)])

(define-type Value [numV (n : Number)] [boolV (b : Boolean)])