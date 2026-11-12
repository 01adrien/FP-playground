#lang plait

(require "types.rkt"
         "parser/lib/core.rkt"
         "parser/lib/combinators.rkt"
         "parser/lib/parsers.rkt")

;; TODO
;; discrimination sur keyword
;; invocation fonction anonyme
;; curry

(define letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define digits "012345678")

(define symbols "!#$%&*+-/<=>?@_~|")

(define paren "(){}[]")

(define blanks " \n\t\r")

(define other ",.:;^`'")

(define id-chars (string-append letters (string-append digits symbols)))

(define readable-chars
  (string-append letters (string-append digits (string-append paren (string-append other symbols)))))

;; parse num EXP
(numP : (Parser Exp))
(define numP
  (Map2 (lambda (a _) (numE a))
        pint
        (Not (pany-char (filter (lambda (c) (not (or (char=? #\] c) (char=? #\} c))))
                                (string->list (string-append letters symbols)))))))

;; parse id
(idP : (Parser Symbol))
(define idP
  (Map2 (lambda (a b) (string->symbol (list->string (cons a b))))
        (pany-char (string->list (string-append letters symbols)))
        (Many (pany-char (string->list id-chars)))))

;; parse var EXP
(varP : (Parser Exp))
(define varP (Map varE idP))

;; parse bool EXP
(boolP : (Parser Exp))
(define boolP
  (Map2 (lambda (a b) (boolE a))
        (Or (Replace (pstring "true") #t) (Replace (pstring "false") #f))
        ;; TODO better
        (Not (pany-char (filter (lambda (c) (not (or (char=? #\] c) (char=? #\} c))))
                                (string->list readable-chars))))))

(define open-curly (And (pchar #\{) (Many pspace)))
(define close-curly (And (pchar #\}) (Many pspace)))

(define (between-curly parser)
  (Between open-curly parser close-curly))

(define open-bracket (And (pchar #\[) (Many pspace)))
(define close-bracket (And (pchar #\]) (Many pspace)))

(define (between-bracket parser)
  (Between open-bracket parser close-bracket))

;; parse une cond EXP
(cndP : (Parser Exp))
(define cndP
  (lazy-parser (lambda ()
                 (between-curly (Map2 (lambda (_ v) (cndE (first v) (second v) (third v)))
                                      (keyP "if")
                                      (Sequence (list expP expP expP)))))))

;; parse a keyword
(keyP : (String -> (Parser Void)))
(define (keyP k)
  (Replace (Skip (Triple (Many pspace) (pstring k) (Many1 pspace))) (void)))

;; parse let binding
(letbindP : (Parser (Listof (Symbol * Exp))))
(define letbindP (lazy-parser (lambda () (between-curly (Many1 (between-bracket (And idP expP)))))))

;; parse let EXP
(letP : (Parser Exp))
(define letP
  (lazy-parser (lambda ()
                 (between-curly (Map3 (lambda (_ b c) (letE b c)) (keyP "let") letbindP expP)))))

;; parse lambda EXP
(lamP : (Parser Exp))
(define lamP
  (lazy-parser (lambda ()
                 (between-curly (Map3 (lambda (_ b c) (lamE b c))
                                      (keyP "lam")
                                      (between-curly (Many (Map2 (lambda (a _) a) idP (Many pspace))))
                                      expP)))))

;; parse application EXP
(appP : (Parser Exp))
(define appP (lazy-parser (lambda () (between-curly (Map2 appE expP (Many expP))))))

;; parse n'importe qu'elle EXP
(expP : (Parser Exp))
(define expP
  (lazy-parser
   (lambda ()
     (Between (Many pspace) (Any (list numP boolP cndP lamP varP letP appP)) (Many pspace)))))

;; point d'entrée pour parser une string du langage
(define (parse str)
  (let ([res (expP str)])
    (if (success? res)
        (success-val res)
        (error 'parse (failure-msg res)))))

;; =====================================================================
;; TESTS PARSER
;; =====================================================================

(print-only-errors #t)
;; hypothèse : (appE fonction (list args ...)) et (lamE (list params ...) corps)
;; les opérateurs + - * / sont de simples applications de (varE '+), (varE '-), ...

;; longs nombres
(test (expP "{+ 123456789012345678901234567890 1}")
      (success (appE (varE '+) (list (numE 123456789012345678901234567890) (numE 1))) ""))

;; ---------- identifiants qui commencent par true / false ----------
(test (expP "truely") (success (varE 'truely) ""))
(test (expP "falseeee") (success (varE 'falseeee) ""))
(test (expP "true1") (success (varE 'true1) ""))
(test (expP "falsey") (success (varE 'falsey) ""))

;; true et false seuls restent des booléens
(test (expP "true rest") (success (boolE #t) "rest"))
(test (expP "false rest") (success (boolE #f) "rest"))

;; en contexte
(test (expP "{if truely 1 2}") (success (cndE (varE 'truely) (numE 1) (numE 2)) ""))
(test (expP "{f true falsey}") (success (appE (varE 'f) (list (boolE #t) (varE 'falsey))) ""))
(test ((Many expP) "true truely false falsey")
      (success (list (boolE #t) (varE 'truely) (boolE #f) (varE 'falsey)) ""))

;; ---------- opérations : +, -, *, / (applications) ----------
;; addition
(test (expP "{+ 5 3}") (success (appE (varE '+) (list (numE 5) (numE 3))) ""))
(test (expP "{+ x 1}") (success (appE (varE '+) (list (varE 'x) (numE 1))) ""))
(test (expP "{+ 5 3} rest") (success (appE (varE '+) (list (numE 5) (numE 3))) "rest"))

;; soustraction
(test (expP "{- 5 3}") (success (appE (varE '-) (list (numE 5) (numE 3))) ""))
(test (expP "{- x 1}") (success (appE (varE '-) (list (varE 'x) (numE 1))) ""))
(test (expP "{- 5 -3}") (success (appE (varE '-) (list (numE 5) (numE -3))) ""))
(test (expP "{- -5 -3}") (success (appE (varE '-) (list (numE -5) (numE -3))) ""))
(test (expP "{-    5     3   }") (success (appE (varE '-) (list (numE 5) (numE 3))) ""))
(test (expP "{ - 5 3}") (success (appE (varE '-) (list (numE 5) (numE 3))) ""))
(test (expP "{- 5 3} rest") (success (appE (varE '-) (list (numE 5) (numE 3))) "rest"))

;; multiplication
(test (expP "{* 2 3}") (success (appE (varE '*) (list (numE 2) (numE 3))) ""))
(test (expP "{* x y}") (success (appE (varE '*) (list (varE 'x) (varE 'y))) ""))
(test (expP "{* -2 3}") (success (appE (varE '*) (list (numE -2) (numE 3))) ""))
(test (expP "{  *  2  3  }") (success (appE (varE '*) (list (numE 2) (numE 3))) ""))
(test (expP "{* 2 3} rest") (success (appE (varE '*) (list (numE 2) (numE 3))) "rest"))

;; division
(test (expP "{/ 6 3}") (success (appE (varE '/) (list (numE 6) (numE 3))) ""))
(test (expP "{/ x 2}") (success (appE (varE '/) (list (varE 'x) (numE 2))) ""))
(test (expP "{/ -6 3}") (success (appE (varE '/) (list (numE -6) (numE 3))) ""))
(test (expP "{/ 1 0}")
      (success (appE (varE '/) (list (numE 1) (numE 0))) "")) ; parse seulement, pas d'évaluation
(test (expP "{  /  6  3  }") (success (appE (varE '/) (list (numE 6) (numE 3))) ""))
(test (expP "{/ 6 3} rest") (success (appE (varE '/) (list (numE 6) (numE 3))) "rest"))

;; ---------- variables : doivent commencer par une lettre ou un symbol ----------
(test (expP "x") (success (varE 'x) ""))
(test (expP "abc") (success (varE 'abc) ""))
(test (expP "Abc") (success (varE 'Abc) ""))
(test (expP "x1") (success (varE 'x1) ""))
(test (expP "x_y") (success (varE 'x_y) ""))
(test (expP "@me") (success (varE '@me) ""))
(test (failure? (expP "1x")) #t) ; ni un nombre, ni une variable
(test (failure? (expP "{let {[5 1]} 5}")) #t) ; un nom lié ne peut pas commencer par un chiffre

;; exception : les opérateurs de la std-lib restent des variables
(test (expP "+") (success (varE '+) ""))
(test (expP "-") (success (varE '-) ""))

;; ---------- nombres : uniquement des chiffres ----------
(test (expP "0") (success (numE 0) ""))
(test (expP "42") (success (numE 42) ""))
(test (expP "-5") (success (numE -5) ""))
(test (expP "42 rest") (success (numE 42) "rest"))
(test (failure? (expP "12ab")) #t) ; des lettres collées : ni nombre, ni variable

;; en contexte
(test (expP "{f x1 2}") (success (appE (varE 'f) (list (varE 'x1) (numE 2))) ""))
(test (failure? (expP "{+ 1x 2}")) #t)

;; plus d'erreur d'arité au parsing : le nombre d'arguments est libre
(test (expP "{+ 1 2 3}") (success (appE (varE '+) (list (numE 1) (numE 2) (numE 3))) ""))
(test (expP "{+ 1}") (success (appE (varE '+) (list (numE 1))) ""))

;; opérateurs mélangés / imbriqués
(test (expP "{+ {* 2 3} {- 5 1}}")
      (success (appE (varE '+)
                     (list (appE (varE '*) (list (numE 2) (numE 3)))
                           (appE (varE '-) (list (numE 5) (numE 1)))))
               ""))
(test (expP "{* {+ 1 2} {+ 3 4}}")
      (success (appE (varE '*)
                     (list (appE (varE '+) (list (numE 1) (numE 2)))
                           (appE (varE '+) (list (numE 3) (numE 4)))))
               ""))
(test (expP "{/ {* 6 2} {- 5 2}}")
      (success (appE (varE '/)
                     (list (appE (varE '*) (list (numE 6) (numE 2)))
                           (appE (varE '-) (list (numE 5) (numE 2)))))
               ""))
(test (expP "{- 10 {- 5 {- 3 1}}}") ; associativité à droite, via les accolades
      (success (appE (varE '-)
                     (list (numE 10)
                           (appE (varE '-)
                                 (list (numE 5) (appE (varE '-) (list (numE 3) (numE 1)))))))
               ""))
(test (expP "{- {- 10 5} 3}")
      (success (appE (varE '-) (list (appE (varE '-) (list (numE 10) (numE 5))) (numE 3))) ""))
(test (expP "{+ 1 {* 2 {- 8 {/ 6 3}}}}") ; les quatre opérateurs imbriqués
      (success (appE (varE '+)
                     (list (numE 1)
                           (appE (varE '*)
                                 (list (numE 2)
                                       (appE (varE '-)
                                             (list (numE 8)
                                                   (appE (varE '/) (list (numE 6) (numE 3)))))))))
               ""))

;; opérations avec les autres formes
(test (expP "{* {if true 2 3} 4}")
      (success (appE (varE '*) (list (cndE (boolE #t) (numE 2) (numE 3)) (numE 4))) ""))
(test (expP "{if {- 1 1} {* 2 3} {/ 6 2}}")
      (success (cndE (appE (varE '-) (list (numE 1) (numE 1)))
                     (appE (varE '*) (list (numE 2) (numE 3)))
                     (appE (varE '/) (list (numE 6) (numE 2))))
               ""))
(test (expP "{let {[x {* 2 3}]} {- x 1}}")
      (success (letE (list (pair 'x (appE (varE '*) (list (numE 2) (numE 3)))))
                     (appE (varE '-) (list (varE 'x) (numE 1))))
               ""))
(test (expP "{let {[a 6] [b 3]} {/ a b}}")
      (success (letE (list (pair 'a (numE 6)) (pair 'b (numE 3)))
                     (appE (varE '/) (list (varE 'a) (varE 'b))))
               ""))
(test (expP "{- x -1}") (success (appE (varE '-) (list (varE 'x) (numE -1))) ""))

;; plusieurs opérations à la suite
(test ((Many expP) "{- 5 3} {* 2 4} {/ 8 2}")
      (success (list (appE (varE '-) (list (numE 5) (numE 3)))
                     (appE (varE '*) (list (numE 2) (numE 4)))
                     (appE (varE '/) (list (numE 8) (numE 2))))
               ""))

;; ---------- application générale : {f arg ...} ----------
(test (expP "{f 1}") (success (appE (varE 'f) (list (numE 1))) ""))
(test (expP "{f 1 2 3}") (success (appE (varE 'f) (list (numE 1) (numE 2) (numE 3))) ""))
(test (expP "{f x y}") (success (appE (varE 'f) (list (varE 'x) (varE 'y))) ""))
(test (expP "{ f  1   2 }") (success (appE (varE 'f) (list (numE 1) (numE 2))) ""))
(test (expP "{f 1} rest") (success (appE (varE 'f) (list (numE 1))) "rest"))
(test (expP "{{f 1} 2}") ; application partielle puis application du résultat
      (success (appE (appE (varE 'f) (list (numE 1))) (list (numE 2))) ""))
(test (expP "{{{f 1} 2} 3}")
      (success (appE (appE (appE (varE 'f) (list (numE 1))) (list (numE 2))) (list (numE 3))) ""))
(test (expP "{f {g 1} {h 2}}")
      (success (appE (varE 'f)
                     (list (appE (varE 'g) (list (numE 1))) (appE (varE 'h) (list (numE 2)))))
               ""))
(test (expP "{f {+ 1 2}}")
      (success (appE (varE 'f) (list (appE (varE '+) (list (numE 1) (numE 2))))) ""))
(test (expP "{f {if true 1 2}}")
      (success (appE (varE 'f) (list (cndE (boolE #t) (numE 1) (numE 2)))) ""))
(test (expP "{f {let {[x 1]} x}}")
      (success (appE (varE 'f) (list (letE (list (pair 'x (numE 1))) (varE 'x)))) ""))
(test (expP "{if {f 1} 2 3}") (success (cndE (appE (varE 'f) (list (numE 1))) (numE 2) (numE 3)) ""))

(test (expP "{f}") (success (appE (varE 'f) (list)) ""))

;; ---------- lambda : {lam param ... corps} ----------
(test (expP "{lam {x} x}") (success (lamE (list 'x) (varE 'x)) ""))
(test (expP "{lam {x} {+ x 1}}")
      (success (lamE (list 'x) (appE (varE '+) (list (varE 'x) (numE 1)))) ""))
(test (expP "{lam {a b c} {+ a {+ b c}}}")
      (success (lamE (list 'a 'b 'c)
                     (appE (varE '+) (list (varE 'a) (appE (varE '+) (list (varE 'b) (varE 'c))))))
               ""))
(test (expP "{ lam  {x   y}  x }") (success (lamE (list 'x 'y) (varE 'x)) ""))
(test (expP "{lam {x} {lam {y} x}}") (success (lamE (list 'x) (lamE (list 'y) (varE 'x))) ""))
(test (expP "{lam {x} {if x 1 2}}") (success (lamE (list 'x) (cndE (varE 'x) (numE 1) (numE 2))) ""))
(test (expP "{lam {x} {let {[y x]} y}}")
      (success (lamE (list 'x) (letE (list (pair 'y (varE 'x))) (varE 'y))) ""))
(test (expP "{lam {x} x} rest") (success (lamE (list 'x) (varE 'x)) "rest"))

;; mots qui ressemblent à lamx
(test (expP "lam") (success (varE 'lam) ""))
(test (expP "lamb") (success (varE 'lamb) ""))

;; à décider selon ton parseur (Many ou Many1 pour les paramètres) :
;;(test (expP "{lam {} 5}") ...)

;; ---------- condition : {if cond then else} ----------
(test (expP "{if true 1 2}") (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{if x 1 2}") (success (cndE (varE 'x) (numE 1) (numE 2)) ""))
(test (expP "{if {+ 1 2} 10 20}") ; la condition est une addition
      (success (cndE (appE (varE '+) (list (numE 1) (numE 2))) (numE 10) (numE 20)) ""))
(test (expP "{if   true   1   2  }") (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{if true {+ 1 2} {+ 3 4}}")
      (success (cndE (boolE #t)
                     (appE (varE '+) (list (numE 1) (numE 2)))
                     (appE (varE '+) (list (numE 3) (numE 4))))
               ""))
(test (expP "{if true {if false 1 2} 3}") ; if imbriqué dans le then
      (success (cndE (boolE #t) (cndE (boolE #f) (numE 1) (numE 2)) (numE 3)) ""))

;; espaces avant le mot-clé
(test (expP "{ if true 1 2}") (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{    if true 1 2}") (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{+ 1 { if true 2 3}}")
      (success (appE (varE '+) (list (numE 1) (cndE (boolE #t) (numE 2) (numE 3)))) ""))

;; ---------- let : {let {[nom expr] ...} corps} (liste de liaisons) ----------
(test (expP "{let {[x 5]} x}") (success (letE (list (pair 'x (numE 5))) (varE 'x)) ""))
(test (expP "{let {[res {+ 1 2}]} {+ res 48}}")
      (success (letE (list (pair 'res (appE (varE '+) (list (numE 1) (numE 2)))))
                     (appE (varE '+) (list (varE 'res) (numE 48))))
               ""))
(test (expP "{let { [x 5] } x}") ; espaces autour du crochet
      (success (letE (list (pair 'x (numE 5))) (varE 'x)) ""))
(test (expP "{let {[x 1]} {let {[y 2]} {+ x y}}}") ; let imbriqué
      (success (letE (list (pair 'x (numE 1)))
                     (letE (list (pair 'y (numE 2))) (appE (varE '+) (list (varE 'x) (varE 'y)))))
               ""))
(test (expP "{let {[x {let {[y 1]} y}]} x}") ; let dans la valeur liée
      (success (letE (list (pair 'x (letE (list (pair 'y (numE 1))) (varE 'y)))) (varE 'x)) ""))
(test (expP "{let {[x 5]} {if true x 0}}") ; mélange let et if
      (success (letE (list (pair 'x (numE 5))) (cndE (boolE #t) (varE 'x) (numE 0))) ""))
(test (expP "{let {[fun {lam {a b c} {+ a {+ b c}}}]} {fun 5 10 15}}")
      (success (letE (list (pair 'fun
                                 (lamE (list 'a 'b 'c)
                                       (appE (varE '+)
                                             (list (varE 'a)
                                                   (appE (varE '+) (list (varE 'b) (varE 'c))))))))
                     (appE (varE 'fun) (list (numE 5) (numE 10) (numE 15))))
               ""))

;; ---------- let avec plusieurs liaisons ----------
(test (expP "{let {[x 1] [y 2]} {+ x y}}")
      (success (letE (list (pair 'x (numE 1)) (pair 'y (numE 2)))
                     (appE (varE '+) (list (varE 'x) (varE 'y))))
               ""))
(test (expP "{let {[a 1] [b 2] [c 3]} {+ a {+ b c}}}") ; trois liaisons, l'ordre est conservé
      (success (letE (list (pair 'a (numE 1)) (pair 'b (numE 2)) (pair 'c (numE 3)))
                     (appE (varE '+) (list (varE 'a) (appE (varE '+) (list (varE 'b) (varE 'c))))))
               ""))
(test (expP "{let {[a {+ 1 2}] [b true]} {if b a 0}}") ; expressions dans les valeurs
      (success (letE (list (pair 'a (appE (varE '+) (list (numE 1) (numE 2)))) (pair 'b (boolE #t)))
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
                                  (appE (varE '+) (list (varE 'a) (varE 'b)))))
                      (pair 'y (numE 3)))
                (appE (varE '+) (list (varE 'x) (varE 'y))))
          ""))
(test (expP "{let {[x 1] [y 2]} {let {[z 3] [w 4]} {+ x z}}}") ; let imbriqué, deux liaisons chacun
      (success (letE (list (pair 'x (numE 1)) (pair 'y (numE 2)))
                     (letE (list (pair 'z (numE 3)) (pair 'w (numE 4)))
                           (appE (varE '+) (list (varE 'x) (varE 'z)))))
               ""))

;; rejets propres aux listes de liaisons
(test (failure? (expP "{let {[x 1] [y]} x}")) #t) ; 2e liaison sans valeur
(test (failure? (expP "{let {[x 1] y 2} x}")) #t) ; 2e liaison sans crochets
(test (failure? (expP "{let {[x 1] [y 2]}}")) #t) ; pas de corps
(test (failure? (expP "{let {[x 1] [y 2} x}")) #t) ; crochet fermant manquant
(test (failure? (expP "{let {[x 1] [y 2]} x")) #t) ; accolade fermante manquante

;; espaces avant le mot-clé
(test (expP "{ let {[x 5]} x}") (success (letE (list (pair 'x (numE 5))) (varE 'x)) ""))
(test (expP "{    let {[x 5]} x}") (success (letE (list (pair 'x (numE 5))) (varE 'x)) ""))

;; ---------- rejets ----------
(test (failure? (expP "")) #t)
(test (failure? (expP "{}")) #t)
(test (failure? (expP "{+ 1 2")) #t) ; accolade fermante manquante
(test (failure? (expP "{let {[x 5]}}")) #t) ; pas de corps
(test (failure? (expP "}")) #t)

(test (expP "{let {[ok {+ 5 6}]} {if ok true false}}")
      (success (letE (list (pair 'ok (appE (varE '+) (list (numE 5) (numE 6)))))
                     (cndE (varE 'ok) (boolE #t) (boolE #f)))
               ""))

;; ---------- expressions dans toutes les positions du if ----------
(test (expP "{if {+ 1 5} {+ 2 3} {+ 4 5}}") ; addition partout
      (success (cndE (appE (varE '+) (list (numE 1) (numE 5)))
                     (appE (varE '+) (list (numE 2) (numE 3)))
                     (appE (varE '+) (list (numE 4) (numE 5))))
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
                     (appE (varE '+) (list (varE 'x) (numE 1)))
                     (appE (varE '+) (list (varE 'x) (numE -1))))
               ""))

;; ---------- le if comme opérande d'une addition ----------
(test (expP "{+ {if true 1 2} 3}")
      (success (appE (varE '+) (list (cndE (boolE #t) (numE 1) (numE 2)) (numE 3))) ""))
(test (expP "{+ {if true 1 2} {if false 3 4}}")
      (success (appE (varE '+)
                     (list (cndE (boolE #t) (numE 1) (numE 2)) (cndE (boolE #f) (numE 3) (numE 4))))
               ""))

;; ---------- let plus riches ----------
(test (expP "{let {[a 1]} {let {[b {+ a 1}]} {+ a b}}}") ; b dépend de a
      (success (letE (list (pair 'a (numE 1)))
                     (letE (list (pair 'b (appE (varE '+) (list (varE 'a) (numE 1)))))
                           (appE (varE '+) (list (varE 'a) (varE 'b)))))
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
      (success (appE (varE '+)
                     (list (numE 1)
                           (appE (varE '+)
                                 (list (numE 2)
                                       (appE (varE '+)
                                             (list (numE 3)
                                                   (appE (varE '+)
                                                         (list (numE 4)
                                                               (appE (varE '+)
                                                                     (list (numE 5) (numE 6)))))))))))
               ""))
(test (expP "{+ -1 -2}") (success (appE (varE '+) (list (numE -1) (numE -2))) ""))

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
(test (failure? (expP "{let {[x]} x}")) #t) ; liaison sans valeur
(test (failure? (expP "{+ 1\n2}")) #t) ; seul l'espace compte comme blanc
(test (failure? (expP "{f 1\n2}")) #t)

;; ---------- encore des comportements actuels à connaître ----------
;; un identifiant peut commencer par un chiffre (pid prend tout id-chars)
(test (failure? (expP "{let {[5 1]} 5}")) #t)
;; un nombre collé à des lettres : le nombre gagne, le reste n'est pas consommé
(test (failure? (expP "1x")) #t)
