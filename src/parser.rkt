#lang plait

(require "types.rkt"
         "parser/lib/core.rkt"
         "parser/lib/combinators.rkt"
         "parser/lib/parsers.rkt")

#|
Expr     <- Num / Bool / If / Plus / Let / Lam / App / Var

Num      <- '-'? [0-9]+ !IdChar Spacing
Bool     <- ('true' / 'false') !IdChar Spacing

If       <- LBRACE IF Expr Expr Expr RBRACE
Plus     <- LBRACE PLUS Expr Expr RBRACE
Let      <- LBRACE LET LBRACE Binding+ RBRACE Expr RBRACE
Binding  <- LBRACK Var Expr RBRACK
Lam      <- LBRACE LAM LBRACE Var* RBRACE Expr RBRACE
App      <- LBRACE Expr Expr+ RBRACE

Var      <- !Keyword [a-zA-Z_] IdChar* Spacing
Keyword  <- ('if' / 'let' / 'lam' / 'true' / 'false') !IdChar
IdChar   <- [a-zA-Z0-9_]

IF       <- 'if'  !IdChar Spacing
LET      <- 'let' !IdChar Spacing
LAM      <- 'lam' !IdChar Spacing
PLUS     <- '+' Spacing
LBRACE   <- '{' Spacing
RBRACE   <- '}' Spacing
LBRACK   <- '[' Spacing
RBRACK   <- ']' Spacing
Spacing  <- ' '*

|#
;; parse num EXP
(numP : (Parser Exp))
(define numP (Map numE pint))

;; parse boolean EXP
(boolP : (Parser Exp))
(define boolP (Map boolE pbool))

;; parse var EXP
(varP : (Parser Exp))
(define varP (Map varE (Map string->symbol pid)))

;; parse plus EXP
(binP : (Parser Exp))
(define binP
  (lazy-parser (lambda ()
                 (Map3 (lambda (a b c) (binE (char->op (snd a)) b c))
                       (And (Skip (Many pspace))
                            (Any (list (pchar #\-) (pchar #\*) (pchar #\/) (pchar #\+))))
                       expP
                       expP))))

;; parse math operation EXP
(opP : (Parser Exp))
(define opP (Between (pchar #\{) binP (pchar #\})))

;; parse une cond EXP
(cndP : (Parser Exp))
(define cndP
  (lazy-parser (lambda ()
                 (Between (pchar #\{)
                          (Map2 (lambda (_ v) (cndE (first v) (second v) (third v)))
                                (keyP "if")
                                (Sequence (list expP expP expP)))
                          (pchar #\})))))

;; parse a keyword
(keyP : (String -> (Parser 'a)))
(define (keyP k)
  (Replace (Skip (Triple (Many pspace) (pstring k) (Many1 pspace))) empty))

;; parse let binding
(letbindP : (Parser (Listof (Symbol * Exp))))
(define letbindP
  (lazy-parser (lambda ()
                 (Between (And (pchar #\{) (Many pspace))
                          (Many1 (Map3 (lambda (a b c) b)
                                       (And (pchar #\[) (Many pspace))
                                       (And (Map string->symbol pid) expP)
                                       (And (pchar #\]) (Many pspace))))
                          (And (pchar #\}) (Many1 pspace))))))

;; parse let EXP
(letP : (Parser Exp))
(define letP
  (lazy-parser (lambda ()
                 (Between (pchar #\{)
                          (Map3 (lambda (_ b c) (letE b c)) (keyP "let") letbindP expP)
                          (pchar #\})))))

;; parse lambda EXP
(lamP : (Parser Exp))
(define lamP
  (lazy-parser (lambda ()
                 (Between (And (pchar #\{) (Many pspace))
                          (Map3 (lambda (_ b c) (lamE b c))
                                (keyP "lam")
                                (Many1 (Map2 (lambda (a _) (string->symbol a)) pid (Many pspace)))
                                expP)
                          (And (pchar #\}) (Many pspace))))))

;; parse application EXP
(appP : (Parser Exp))
(define appP
  (lazy-parser (lambda ()
                 (Between (And (pchar #\{) (Many pspace))
                          (Map2 appE expP (Many1 expP))
                          (And (pchar #\}) (Many pspace))))))

;; parse n'importe qu'elle EXP
(expP : (Parser Exp))
(define expP
  (lazy-parser
   (lambda ()
     (Between (Many pspace) (Any (list numP opP boolP cndP varP letP lamP appP)) (Many pspace)))))


(define (parse str)
  (let ([res (expP str)])
    (if (success? res)
        (success-val res)
        (error 'parse (failure-msg res)))))


;; =====================================================================
;; TESTS PARSER
;; =====================================================================

(print-only-errors #t)

;; ---------- nombres ----------
(test (expP "5") (success (numE 5) ""))
(test (expP "42") (success (numE 42) ""))
(test (expP "-6") (success (numE -6) ""))
(test (expP "1234567890") (success (numE 1234567890) ""))
(test (expP "5abc") (success (numE 5) "abc"))

;; ---------- booléens ----------
(test (expP "true") (success (boolE #t) ""))
(test (expP "false") (success (boolE #f) ""))

;; ---------- variables ----------
(test (expP "x") (success (varE 'x) ""))
(test (expP "foo_bar") (success (varE 'foo_bar) ""))
(test (expP "x1") (success (varE 'x1) "")) ; chiffre après une lettre : variable
(test (expP "res") (success (varE 'res) ""))

;; ---------- espaces autour d'une expression ----------
(test (expP "  5") (success (numE 5) ""))
(test (expP "5  ") (success (numE 5) ""))
(test (expP "  x  ") (success (varE 'x) ""))
(test (expP "x y") (success (varE 'x) "y")) ; consomme l'espace, s'arrête avant y

;; ---------- addition ----------
(test (expP "{+ 1 2}") (success (binE (plus) (numE 1) (numE 2)) ""))
(test (expP "{+ -6 5}") (success (binE (plus) (numE -6) (numE 5)) ""))
(test (expP "{+ x 1}") (success (binE (plus) (varE 'x) (numE 1)) ""))
(test (expP "{+ true 1}") (success (binE (plus) (boolE #t) (numE 1)) "")) ; pas de contrôle de type ici
(test (expP "{+ 1 2} rest") (success (binE (plus) (numE 1) (numE 2)) "rest"))

;; espaces dans l'addition
(test (expP "{  + 1 2}") (success (binE (plus) (numE 1) (numE 2)) ""))
(test (expP "{+    1     2   }") (success (binE (plus) (numE 1) (numE 2)) ""))
(test (expP "{ + 1 2}") (success (binE (plus) (numE 1) (numE 2)) ""))

;; imbrication
(test (expP "{+ 5 {+ 6 {+ 7 8}}}")
      (success (binE (plus) (numE 5)
                     (binE (plus) (numE 6)
                           (binE (plus) (numE 7) (numE 8))))
               ""))
(test (expP "{+ {+ 1 2} {+ 3 4}}")
      (success (binE (plus)
                     (binE (plus) (numE 1) (numE 2))
                     (binE (plus) (numE 3) (numE 4)))
               ""))
(test (expP "{+ 555 {+ -6 {+ 789 true}}}")
      (success (binE (plus) (numE 555)
                     (binE (plus) (numE -6)
                           (binE (plus) (numE 789) (boolE #t))))
               ""))

;; longs nombres
(test (expP "{+ 123456789012345678901234567890 1}")
      (success (binE (plus) (numE 123456789012345678901234567890) (numE 1)) ""))

;; ---------- opérations binaires : -, *, / ----------
;; soustraction
(test (expP "{- 5 3}") (success (binE (minus) (numE 5) (numE 3)) ""))
(test (expP "{- x 1}") (success (binE (minus) (varE 'x) (numE 1)) ""))
(test (expP "{- 5 -3}") (success (binE (minus) (numE 5) (numE -3)) ""))
(test (expP "{- -5 -3}") (success (binE (minus) (numE -5) (numE -3)) ""))
(test (expP "{-    5     3   }") (success (binE (minus) (numE 5) (numE 3)) ""))
(test (expP "{ - 5 3}") (success (binE (minus) (numE 5) (numE 3)) ""))
(test (expP "{- 5 3} rest") (success (binE (minus) (numE 5) (numE 3)) "rest"))

;; multiplication
(test (expP "{* 2 3}") (success (binE (mult) (numE 2) (numE 3)) ""))
(test (expP "{* x y}") (success (binE (mult) (varE 'x) (varE 'y)) ""))
(test (expP "{* -2 3}") (success (binE (mult) (numE -2) (numE 3)) ""))
(test (expP "{  *  2  3  }") (success (binE (mult) (numE 2) (numE 3)) ""))
(test (expP "{* 2 3} rest") (success (binE (mult) (numE 2) (numE 3)) "rest"))

;; division
(test (expP "{/ 6 3}") (success (binE (div) (numE 6) (numE 3)) ""))
(test (expP "{/ x 2}") (success (binE (div) (varE 'x) (numE 2)) ""))
(test (expP "{/ -6 3}") (success (binE (div) (numE -6) (numE 3)) ""))
(test (expP "{/ 1 0}") (success (binE (div) (numE 1) (numE 0)) "")) ; parse seulement, pas d'évaluation
(test (expP "{  /  6  3  }") (success (binE (div) (numE 6) (numE 3)) ""))
(test (expP "{/ 6 3} rest") (success (binE (div) (numE 6) (numE 3)) "rest"))

;; opérateurs mélangés / imbriqués
(test (expP "{+ {* 2 3} {- 5 1}}")
      (success (binE (plus)
                     (binE (mult) (numE 2) (numE 3))
                     (binE (minus) (numE 5) (numE 1)))
               ""))
(test (expP "{* {+ 1 2} {+ 3 4}}")
      (success (binE (mult)
                     (binE (plus) (numE 1) (numE 2))
                     (binE (plus) (numE 3) (numE 4)))
               ""))
(test (expP "{/ {* 6 2} {- 5 2}}")
      (success (binE (div)
                     (binE (mult) (numE 6) (numE 2))
                     (binE (minus) (numE 5) (numE 2)))
               ""))
(test (expP "{- 10 {- 5 {- 3 1}}}") ; associativité à droite, via les accolades
      (success (binE (minus) (numE 10)
                     (binE (minus) (numE 5)
                           (binE (minus) (numE 3) (numE 1))))
               ""))
(test (expP "{- {- 10 5} 3}")
      (success (binE (minus)
                     (binE (minus) (numE 10) (numE 5))
                     (numE 3))
               ""))
(test (expP "{+ 1 {* 2 {- 8 {/ 6 3}}}}") ; les quatre opérateurs imbriqués
      (success (binE (plus) (numE 1)
                     (binE (mult) (numE 2)
                           (binE (minus) (numE 8)
                                 (binE (div) (numE 6) (numE 3)))))
               ""))

;; opérations binaires avec les autres formes
(test (expP "{* {if true 2 3} 4}")
      (success (binE (mult) (cndE (boolE #t) (numE 2) (numE 3)) (numE 4)) ""))
(test (expP "{if {- 1 1} {* 2 3} {/ 6 2}}")
      (success (cndE (binE (minus) (numE 1) (numE 1))
                     (binE (mult) (numE 2) (numE 3))
                     (binE (div) (numE 6) (numE 2)))
               ""))
(test (expP "{let {[x {* 2 3}]} {- x 1}}")
      (success (letE (list (pair 'x (binE (mult) (numE 2) (numE 3))))
                     (binE (minus) (varE 'x) (numE 1)))
               ""))
(test (expP "{let {[a 6] [b 3]} {/ a b}}")
      (success (letE (list (pair 'a (numE 6)) (pair 'b (numE 3)))
                     (binE (div) (varE 'a) (varE 'b)))
               ""))
(test (expP "{- x -1}") (success (binE (minus) (varE 'x) (numE -1)) ""))

;; plusieurs opérations à la suite
(test ((Many expP) "{- 5 3} {* 2 4} {/ 8 2}")
      (success (list (binE (minus) (numE 5) (numE 3))
                     (binE (mult) (numE 2) (numE 4))
                     (binE (div) (numE 8) (numE 2)))
               ""))


;; ---------- condition : {if cond then else} ----------
(test (expP "{if true 1 2}") (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{if x 1 2}") (success (cndE (varE 'x) (numE 1) (numE 2)) ""))
(test (expP "{if {+ 1 2} 10 20}") ; la condition est une addition
      (success (cndE (binE (plus) (numE 1) (numE 2)) (numE 10) (numE 20)) ""))
(test (expP "{if   true   1   2  }") (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{if true {+ 1 2} {+ 3 4}}")
      (success (cndE (boolE #t)
                     (binE (plus) (numE 1) (numE 2))
                     (binE (plus) (numE 3) (numE 4)))
               ""))
(test (expP "{if true {if false 1 2} 3}") ; if imbriqué dans le then
      (success (cndE (boolE #t) (cndE (boolE #f) (numE 1) (numE 2)) (numE 3)) ""))

;; espaces avant le mot-clé
(test (expP "{ if true 1 2}") (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{    if true 1 2}") (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{+ 1 { if true 2 3}}")
      (success (binE (plus) (numE 1) (cndE (boolE #t) (numE 2) (numE 3))) ""))

;; ---------- let : {let {[nom expr] ...} corps} (liste de liaisons) ----------
(test (expP "{let {[x 5]} x}") (success (letE (list (pair 'x (numE 5))) (varE 'x)) ""))
(test (expP "{let {[res {+ 1 2}]} {+ res 48}}")
      (success (letE (list (pair 'res (binE (plus) (numE 1) (numE 2))))
                     (binE (plus) (varE 'res) (numE 48)))
               ""))
(test (expP "{let { [x 5] } x}") ; espaces autour du crochet
      (success (letE (list (pair 'x (numE 5))) (varE 'x)) ""))
(test (expP "{let {[x 1]} {let {[y 2]} {+ x y}}}") ; let imbriqué
      (success (letE (list (pair 'x (numE 1)))
                     (letE (list (pair 'y (numE 2))) (binE (plus) (varE 'x) (varE 'y))))
               ""))
(test (expP "{let {[x {let {[y 1]} y}]} x}") ; let dans la valeur liée
      (success (letE (list (pair 'x (letE (list (pair 'y (numE 1))) (varE 'y)))) (varE 'x)) ""))
(test (expP "{let {[x 5]} {if true x 0}}") ; mélange let et if
      (success (letE (list (pair 'x (numE 5))) (cndE (boolE #t) (varE 'x) (numE 0))) ""))

;; ---------- let avec plusieurs liaisons ----------
(test (expP "{let {[x 1] [y 2]} {+ x y}}")
      (success (letE (list (pair 'x (numE 1)) (pair 'y (numE 2)))
                     (binE (plus) (varE 'x) (varE 'y)))
               ""))
(test (expP "{let {[a 1] [b 2] [c 3]} {+ a {+ b c}}}") ; trois liaisons, l'ordre est conservé
      (success (letE (list (pair 'a (numE 1)) (pair 'b (numE 2)) (pair 'c (numE 3)))
                     (binE (plus) (varE 'a) (binE (plus) (varE 'b) (varE 'c))))
               ""))
(test (expP "{let {[a {+ 1 2}] [b true]} {if b a 0}}") ; expressions dans les valeurs
      (success (letE (list (pair 'a (binE (plus) (numE 1) (numE 2))) (pair 'b (boolE #t)))
                     (cndE (varE 'b) (varE 'a) (numE 0)))
               ""))
(test (expP "{let { [x 1]   [y 2] } x}") ; espaces autour et entre les liaisons
      (success (letE (list (pair 'x (numE 1)) (pair 'y (numE 2))) (varE 'x)) ""))
(test (expP "{let {[x 1] [x 2]} x}") ; doublon : parse seulement, pas d'évaluation
      (success (letE (list (pair 'x (numE 1)) (pair 'x (numE 2))) (varE 'x)) ""))
(test
 (expP "{let {[x {let {[a 1] [b 2]} {+ a b}}] [y 3]} {+ x y}}") ; let à 2 liaisons dans une valeur
 (success (letE (list (pair 'x
                            (letE (list (pair 'a (numE 1)) (pair 'b (numE 2)))
                                  (binE (plus) (varE 'a) (varE 'b))))
                      (pair 'y (numE 3)))
                (binE (plus) (varE 'x) (varE 'y)))
          ""))
(test (expP "{let {[x 1] [y 2]} {let {[z 3] [w 4]} {+ x z}}}") ; let imbriqué, deux liaisons chacun
      (success (letE (list (pair 'x (numE 1)) (pair 'y (numE 2)))
                     (letE (list (pair 'z (numE 3)) (pair 'w (numE 4)))
                           (binE (plus) (varE 'x) (varE 'z))))
               ""))

;; rejets propres aux listes de liaisons
(test (failure? (expP "{let {[x 1] [y]} x}")) #t) ; 2e liaison sans valeur
(test (failure? (expP "{let {[x 1] y 2} x}")) #t) ; 2e liaison sans crochets
(test (failure? (expP "{let {[x 1] [y 2]}}")) #t) ; pas de corps
(test (failure? (expP "{let {[x 1] [y 2} x}")) #t) ; crochet fermant manquant
(test (failure? (expP "{let {[x 1] [y 2]} x")) #t) ; accolade fermante manquante

;; à décider selon ton parseur (Many ou Many1 pour les liaisons) :
;;(test (expP "{let {} 5}") (success (letE (list) (numE 5)) "")) ; liste vide ?
;;(test (expP "{let {[x 1][y 2]} x}") ...)                      ; liaisons collées ?

;; espaces avant le mot-clé
(test (expP "{ let {[x 5]} x}") (success (letE (list (pair 'x (numE 5))) (varE 'x)) ""))
(test (expP "{    let {[x 5]} x}") (success (letE (list (pair 'x (numE 5))) (varE 'x)) ""))

;; ---------- plusieurs expressions de suite ----------
(test ((Many expP) "1 2 3") (success (list (numE 1) (numE 2) (numE 3)) ""))
(test ((Many expP) "{+ 1 2} 3 x")
      (success (list (binE (plus) (numE 1) (numE 2)) (numE 3) (varE 'x)) ""))

;; ---------- rejets ----------
(test (failure? (expP "")) #t)
(test (failure? (expP "{}")) #t)
(test (failure? (expP "{+ 1 2")) #t) ; accolade fermante manquante
(test (failure? (expP "{let {[x 5]}}")) #t) ; pas de corps
(test (failure? (expP "}")) #t)

;; ---------- comportements actuels à connaître (à corriger) ----------
;; un mot qui commence par true/false est lu comme un booléen suivi d'un reste
(test (expP "trueish") (success (boolE #t) "ish"))
;;(test (expP "falsey") (success (boolE #f) "ey"))
;; la liaison du let exige au moins un espace après "}" (Many1)
(test (failure? (expP "{let {[x 5]}x}")) #t)

;; ---------- ton exemple, avec la syntaxe de ce parseur ({} et true/false) ----------
(test (expP "{let {[ok {+ 5 6}]} {if ok true false}}")
      (success (letE (list (pair 'ok (binE (plus) (numE 5) (numE 6))))
                     (cndE (varE 'ok) (boolE #t) (boolE #f)))
               ""))

;; ---------- expressions dans toutes les positions du if ----------
(test (expP "{if {+ 1 5} {+ 2 3} {+ 4 5}}") ; addition partout
      (success (cndE (binE (plus) (numE 1) (numE 5))
                     (binE (plus) (numE 2) (numE 3))
                     (binE (plus) (numE 4) (numE 5)))
               ""))
(test (expP "{if {if true false true} 1 2}") ; if dans la condition
      (success (cndE (cndE (boolE #t) (boolE #f) (boolE #t)) (numE 1) (numE 2)) ""))
(test (expP "{if false 1 {if true 2 3}}") ; if dans le else
      (success (cndE (boolE #f) (numE 1) (cndE (boolE #t) (numE 2) (numE 3))) ""))
(test (expP "{if {let {[x true]} x} 1 2}") ; let dans la condition
      (success (cndE (letE (list (pair 'x (boolE #t))) (varE 'x)) (numE 1) (numE 2)) ""))
(test (expP "{if true {let {[x 1]} x} 2}") ; let dans le then
      (success (cndE (boolE #t) (letE (list (pair 'x (numE 1))) (varE 'x)) (numE 2)) ""))
(test (expP "{if x {+ x 1} {+ x -1}}") ; variables et négatifs
      (success (cndE (varE 'x)
                     (binE (plus) (varE 'x) (numE 1))
                     (binE (plus) (varE 'x) (numE -1)))
               ""))

;; ---------- le if comme opérande d'une addition ----------
(test (expP "{+ {if true 1 2} 3}")
      (success (binE (plus) (cndE (boolE #t) (numE 1) (numE 2)) (numE 3)) ""))
(test (expP "{+ {if true 1 2} {if false 3 4}}")
      (success (binE (plus)
                     (cndE (boolE #t) (numE 1) (numE 2))
                     (cndE (boolE #f) (numE 3) (numE 4)))
               ""))

;; ---------- let plus riches ----------
(test (expP "{let {[a 1]} {let {[b {+ a 1}]} {+ a b}}}") ; b dépend de a
      (success (letE (list (pair 'a (numE 1)))
                     (letE (list (pair 'b (binE (plus) (varE 'a) (numE 1))))
                           (binE (plus) (varE 'a) (varE 'b))))
               ""))
(test (expP "{let {[x 1]} {let {[x 2]} x}}") ; masquage : parse seulement, pas d'évaluation
      (success (letE (list (pair 'x (numE 1))) (letE (list (pair 'x (numE 2))) (varE 'x))) ""))
(test (expP "{let {[b true]} b}") (success (letE (list (pair 'b (boolE #t))) (varE 'b)) ""))
(test (expP "{let {[y x]} y}") (success (letE (list (pair 'y (varE 'x))) (varE 'y)) ""))
(test (expP "{let {[long_name_123 5]} long_name_123}")
      (success (letE (list (pair 'long_name_123 (numE 5))) (varE 'long_name_123)) ""))

;; ---------- espaces partout ----------
(test (expP "{  let  {  [  x  5  ]  }  x  }") (success (letE (list (pair 'x (numE 5))) (varE 'x)) ""))
(test (expP "{  if  x  1  2  }") (success (cndE (varE 'x) (numE 1) (numE 2)) ""))
(test (expP "  {if true 1 2}  suite") ; espaces autour, le reste est conservé
      (success (cndE (boolE #t) (numE 1) (numE 2)) "suite"))

;; ---------- imbrication profonde ----------
(test (expP "{+ 1 {+ 2 {+ 3 {+ 4 {+ 5 6}}}}}")
      (success (binE (plus) (numE 1)
                     (binE (plus) (numE 2)
                           (binE (plus) (numE 3)
                                 (binE (plus) (numE 4)
                                       (binE (plus) (numE 5) (numE 6)))))) 
               ""))
(test (expP "{+ -1 -2}") (success (binE (plus) (numE -1) (numE -2)) ""))

;; ---------- mots qui ressemblent à des mots-clés ----------
(test (expP "letter") (success (varE 'letter) "")) ; let n'est un mot-clé que dans {let ...}
(test (expP "iffy") (success (varE 'iffy) ""))
(test (expP "if") (success (varE 'if) "")) ; pour l'instant, if seul est une variable

;; ---------- plusieurs expressions de suite, mélangées ----------
(test ((Many expP) "{let {[x 1]} x} {if true 1 2} 3")
      (success
       (list (letE (list (pair 'x (numE 1))) (varE 'x)) (cndE (boolE #t) (numE 1) (numE 2)) (numE 3))
       ""))

;; ---------- rejets supplémentaires ----------
(test (failure? (expP "(+ 5 6)")) #t) ; parenthèses : pas la syntaxe
(test (failure? (expP "#t")) #t) ; #t n'existe pas, c'est true
(test (failure? (expP "{if ok #t #f}")) #t)
(test (failure? (expP "{let {[x]} x}")) #t) ; liaison sans valeur
(test (failure? (expP "{if}")) #t)
(test (failure? (expP "{+ 1\n2}")) #t) ; seul l'espace compte comme blanc

;; ---------- encore des comportements actuels à connaître ----------
;; un identifiant peut commencer par un chiffre (pid prend tout id-chars)
(test (expP "{let {[5 1]} 5}")
      (success (letE (list (pair (string->symbol "5") (numE 1))) (numE 5)) ""))
;; un nombre collé à des lettres : le nombre gagne, le reste n'est pas consommé
(test (expP "1x") (success (numE 1) "x"))