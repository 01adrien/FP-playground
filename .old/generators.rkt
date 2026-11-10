#lang plait

(require "../lib/combinators.rkt"
         "../lib/parsers.rkt"
         "../lib/utils.rkt"
         "../lib/core.rkt")

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

;; ----
;; AST
;; ----

;; Tree type
(define-type Tree [leaf (text : String)] [node (ref : Symbol) (children : (Listof Tree))])

;; (parser-lang->tree l)
(parser-lang->tree : (Lang -> (Parser (Listof Tree))))
(define (parser-lang->tree l)
  (parse-rule l (ref (fst (first l)))))

;; (parse-rule l r)
;; transforme une lang en parse tree de PEG 'compilé'
;; faire une cache pour les rules en ref pour eviter de les parser a chaque fois ??
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

;; ---------------------
;; Tree -> Lang
;; ---------------------

(blank? : (Tree -> Boolean))
(define (blank? t)
  (type-case Tree
             t
             [(leaf str) (string=? str " ")]
             [(node ref _) (member ref (list 'Spacing 'Space 'EndOfLine 'EndOfFile))]))

(drop-blanks : ((Listof Tree) -> (Listof Tree)))
(define (drop-blanks ts)
  (filter (lambda (t) (not (blank? t))) ts))

;; Grammar <- Spacing Definition+ EndOfFile
(tree->lang : (Tree -> Lang))
(define (tree->lang t)
  (map tree->rule (drop-blanks (node-children t))))

;; Definition <- Identifier LEFTARROW Expression
(tree->rule : (Tree -> Rule))
(define (tree->rule t)
  (pair (string->symbol (tree->identifier (first (node-children t))))
        (tree->expression (drop-blanks (rest (rest (node-children t)))))))

;; Identifier <- IdentStart IdentCont* Spacing
(tree->identifier : (Tree -> String))
(define (tree->identifier t)
  (type-case Tree
             t
             [(leaf str) str]
             [(node ref children)
              (foldr (lambda (curr acc) (string-append (tree->identifier curr) acc))
                     ""
                     (drop-blanks children))]))

;; Expression <- Sequence (SLASH Sequence)*
(tree->expression : (Tree -> Grammar))
(define (tree->expression t)
  (begin (display t) (eps)))

;; Sequence <- Prefix*
(tree->sequence : (Tree -> Grammar))
(define (tree->sequence t)
  (begin (display t) (eps)))

;; Prefix <- (AND / NOT)? Suffix
(tree->prefix : (Tree -> Grammar))
(define (tree->prefix t)
  (begin (display t) (eps)))

;; Suffix <- Primary (QUESTION / STAR / PLUS)?
(tree->suffix : (Tree -> Grammar))
(define (tree->suffix t)
  (begin (display t) (eps)))

;; Primary <- Identifier !LEFTARROW / OPEN Expression CLOSE / Literal / Class / DOT
(tree->primary : (Tree -> Grammar))
(define (tree->primary t)
  (begin (display t) (eps)))

;; Literal <- ['] (!['] Char)* ['] Spacing / ...
(tree->literal : (Tree -> Grammar))
(define (tree->literal t)
  (begin (display t) (eps)))

;; Class <- '[' (!']' Range)* ']' Spacing
(tree->class : (Tree -> Grammar))
(define (tree->class t)
  (begin (display t) (eps)))

;; --- catégories qui NE produisent PAS une Grammar ---

;; Char -> le caractère décodé (gère \n, \', \012, ...)
(tree->char : (Tree -> String))
(define (tree->char t)
  (error 'tree->char "TODO"))

;; (leaves->string t) : concatène le texte de toutes les feuilles sous t
(leaves->string : (Tree -> String))
(define (leaves->string t)
  (error 'leaves->string "TODO"))

;; ---------------------
;; Tree pretty printer
;; ---------------------

;; (pp-tree res)
(pp-tree : ((ParseResult (Listof Tree)) -> Void))
(define (pp-tree res)
  (type-case (ParseResult (Listof Tree))
             res
             [(success val rem)
              (begin
                (display "\nTREE\n")
                (display (tree->string (first val) 0))
                (display "\nREM\n")
                (display rem))]
             [(failure msg) (display msg)]))

;; (tree->string tree i)
(tree->string : (Tree Number -> String))
(define (tree->string tree i)
  (type-case Tree
             tree
             [(leaf text) (string-concat (list (indent i) "leaf '" text "'\n"))]
             [(node ref children)
              (if (equal? ref 'Spacing)
                  ""
                  (string-concat (list (indent i)
                                       (symbol->string ref)
                                       "\n"
                                       (foldr string-append
                                              ""
                                              (map (lambda (child) (tree->string child (+ i 1)))
                                                   children)))))]))

;; (indent n)
(indent : (Number -> String))
(define (indent n)
  (if (<= n 0)
      ""
      (string-append " " (indent (- n 1)))))

;; (all-leaves? trees)
(all-leaves? : ((Listof Tree) -> Boolean))
(define (all-leaves? trees)
  (foldl (lambda (curr acc) (and (leaf? curr) acc)) #t trees))
