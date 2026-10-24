#lang plait

(require "lib/combinators.rkt"
         "lib/parsers.rkt"
         "lib/utils.rkt"
         "lib/core.rkt")

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

;; (find-rule name lang)
;; cherche la règle de nom donné dans la Lang.
(find-rule : (Symbol Lang -> (Optionof Grammar)))
(define (find-rule name lang)
  (type-case (Listof Rule)
             lang
             [empty (none)]
             [(cons rule others)
              (if (equal? name (fst rule))
                  (some (snd rule))
                  (find-rule name others))]))

;; faire une cache pour les rules en ref pour eviter de les parser a chaque fois
(parse-rule-str : (Lang Grammar -> (Parser String)))
(define (parse-rule-str l r)
  (type-case Grammar
             r
             [(eps) (return "")]
             [(lit text) (pstring text)]
             [(ref name)
              (type-case (Optionof Grammar)
                         (find-rule name l)
                         [(none) (error 'ref "rèf inconnue")] ; cas absent
                         [(some r) (lazy-parser (lambda () (parse-rule-str l r)))])]
             [(seq parts)
              (Map (lambda (s) (string-concat s))
                   (Sequence (map (lambda (r) (parse-rule-str l r)) parts)))]
             [(alt choices) (Any (map (lambda (r) (parse-rule-str l r)) choices))]
             [(star g) (Map (lambda (s) (string-concat s)) (Many (parse-rule-str l g)))]
             [(not-p g) (Map (lambda (_) "") (Not (parse-rule-str l g)))]))

(parser-str : (Lang -> (Parser String)))
(define (parser-str l)
  (parse-rule-str l (snd (first l))))

(define-type Tree
             [empty-tree]
             [leaf (text : String)]
             [node (ref : Symbol) (children : (Listof Tree))])

(parse-rule : (Lang Grammar -> (Parser (Listof Tree))))
(define (parse-rule l r)
  (type-case
   Grammar
   r
   [(lit text) (Map (lambda (x) (list (leaf x))) (pstring text))]
   [(eps) (return (list))]
   [(ref name)
    (type-case (Optionof Grammar)
               (find-rule name l)
               [(none) (error 'ref "rèf inconnue")]
               [(some r)
                (lazy-parser (lambda () (Map (lambda (c) (list (node name c))) (parse-rule l r))))])]
   [(seq parts)
    (Map (lambda (ls) (foldr append (list) ls)) (Sequence (map (lambda (p) (parse-rule l p)) parts)))]
   [(alt choices) (Any (map (lambda (r) (parse-rule l r)) choices))]
   [(star g) (Map (lambda (ls) (foldr append (list) ls)) (Many (parse-rule l g)))]
   [(not-p g) (Map (lambda (_) (list)) (Not (parse-rule l g)))]))

;; A <- '5' / 'hello'

;; (define test (list (pair 'A (alt (list (lit "5") (lit "hello"))))))
;;
;; (define p ( parse-rule test (snd (first test))))
;; (p "5")

(define (parser l)
  (parse-rule l (ref (fst (first l)))))