#lang plait

(require "types.rkt" 
         "parser/lib/core.rkt"
         "parser/lib/combinators.rkt"
         "parser/lib/parsers.rkt")

; GRAMMAR
; <expr> ::= <num>
;          | <bool>
;          | {if <expr> <expr> <expr> }
;          | {+ <expr> <expr>}
;          | {let1 {[<var> <expr>]} <expr>}
;          | <var>


;; parse num EXP
(numP : (Parser Exp))
(define numP (Map numE pint))

;; parse boolean EXP
(boolP : (Parser Exp))
(define boolP (Map boolE pbool))

;; parse var EXP
(varP : (Parser Exp))
(define varP  
  (Map varE (Map string->symbol pid)))

;; helper pour parse plus EXP
(mkplusP : ((Char * (Exp * Exp)) -> Exp))
(define (mkplusP v) 
  (plusE (fst (snd v)) (snd (snd v))))

;; parse plus EXP
(plusP : (Parser Exp))
(define plusP 
  (lazy-parser
    (lambda ()
      (Map mkplusP 
        (Triple 
          (And (Skip (Many pspace)) (pchar #\+)) 
          expP 
          expP)))))

;; parse math operation EXP
(opP : (Parser Exp))
(define opP 
  (Between (pchar #\{) plusP (pchar #\})))

;; parse if keyword (partie de cond EXP)
(ifkeyP : (Parser Void))
(define ifkeyP
  (Skip (Triple (Many pspace) (pstring "if") (Many1 pspace))))


;; parse une cond EXP
(cndP : (Parser Exp))
(define cndP
  (lazy-parser
   (lambda ()
     (Between
      (pchar #\{)
      (Map (lambda (v) (cndE (first v) (second v) (third v)))
           (Map snd (And ifkeyP (Sequence (list expP expP expP)))))
      (pchar #\})))))


;; parse let keyword (partie de let1 EXP)
(letkeyP : (Parser Void))
(define letkeyP
  (Skip (Triple (Many pspace) (pstring "let") (Many1 pspace))))

;; parse let binding
(letbindP : (Parser (Symbol * Exp)))
(define letbindP
(lazy-parser
  (lambda ()  
    (Between 
      (Quad (pchar #\{) (Many pspace) (pchar #\[) (Many pspace))
      (And (Map string->symbol pid) expP) 
      (Quad (pchar #\]) (Many pspace) (pchar #\}) (Many1 pspace))))))

;; parse let EXP
(letP : (Parser Exp))
(define letP
  (lazy-parser
   (lambda ()
     (Between
      (pchar #\{)
      (Map (lambda (v) (let1E (fst (fst v)) (snd (fst v)) (snd v)))
           (Map snd (Triple letkeyP letbindP expP)))
      (pchar #\})))))

;; parse n'importe qu'elle EXP
(expP : (Parser Exp))
(define expP
  (lazy-parser 
    (lambda () 
      (Between 
        (Many pspace) 
          (Any (list numP opP boolP cndP varP letP)) 
        (Many pspace)))))


(define (parse str) 
(let ([res (expP str)])
           (if (success? res)  
            (success-val res) 
            (error 'parse "cannot parse"))))

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
(test (expP "x1") (success (varE 'x1) ""))   ; chiffre après une lettre : variable
(test (expP "res") (success (varE 'res) ""))

;; ---------- espaces autour d'une expression ----------
(test (expP "  5") (success (numE 5) ""))
(test (expP "5  ") (success (numE 5) ""))
(test (expP "  x  ") (success (varE 'x) ""))
(test (expP "x y") (success (varE 'x) "y"))   ; consomme l'espace, s'arrête avant y

;; ---------- addition ----------
(test (expP "{+ 1 2}") (success (plusE (numE 1) (numE 2)) ""))
(test (expP "{+ -6 5}") (success (plusE (numE -6) (numE 5)) ""))
(test (expP "{+ x 1}") (success (plusE (varE 'x) (numE 1)) ""))
(test (expP "{+ true 1}") (success (plusE (boolE #t) (numE 1)) ""))   ; pas de contrôle de type ici
(test (expP "{+ 1 2} rest") (success (plusE (numE 1) (numE 2)) "rest"))

;; espaces dans l'addition
(test (expP "{  + 1 2}") (success (plusE (numE 1) (numE 2)) ""))
(test (expP "{+    1     2   }") (success (plusE (numE 1) (numE 2)) ""))
(test (expP "{ + 1 2}") (success (plusE (numE 1) (numE 2)) ""))

;; imbrication
(test (expP "{+ 5 {+ 6 {+ 7 8}}}")
      (success (plusE (numE 5) (plusE (numE 6) (plusE (numE 7) (numE 8)))) ""))
(test (expP "{+ {+ 1 2} {+ 3 4}}")
      (success (plusE (plusE (numE 1) (numE 2)) (plusE (numE 3) (numE 4))) ""))
(test (expP "{+ 555 {+ -6 {+ 789 true}}}")
      (success (plusE (numE 555) (plusE (numE -6) (plusE (numE 789) (boolE #t)))) ""))

;; longs nombres
(test (expP "{+ 123456789012345678901234567890 1}")
      (success (plusE (numE 123456789012345678901234567890) (numE 1)) ""))

;; ---------- condition : {if cond then else} ----------
(test (expP "{if true 1 2}")
      (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{if x 1 2}")
      (success (cndE (varE 'x) (numE 1) (numE 2)) ""))
(test (expP "{if {+ 1 2} 10 20}")   ; la condition est une addition
      (success (cndE (plusE (numE 1) (numE 2)) (numE 10) (numE 20)) ""))
(test (expP "{if   true   1   2  }")
      (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{if true {+ 1 2} {+ 3 4}}")
      (success (cndE (boolE #t) (plusE (numE 1) (numE 2)) (plusE (numE 3) (numE 4))) ""))
(test (expP "{if true {if false 1 2} 3}")   ; if imbriqué dans le then
      (success (cndE (boolE #t) (cndE (boolE #f) (numE 1) (numE 2)) (numE 3)) ""))

;; espaces avant le mot-clé
(test (expP "{ if true 1 2}")
      (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{    if true 1 2}")
      (success (cndE (boolE #t) (numE 1) (numE 2)) ""))
(test (expP "{+ 1 { if true 2 3}}")
      (success (plusE (numE 1) (cndE (boolE #t) (numE 2) (numE 3))) ""))
(test (failure? (expP "{iffy true 1 2}")) #t)   ; "if" doit être suivi d'un espace

;; ---------- let : {let {[nom expr]} corps} ----------
(test (expP "{let {[x 5]} x}")
      (success (let1E 'x (numE 5) (varE 'x)) ""))
(test (expP "{let {[res {+ 1 2}]} {+ res 48}}")
      (success (let1E 'res (plusE (numE 1) (numE 2)) (plusE (varE 'res) (numE 48))) ""))
(test (expP "{let { [x 5] } x}")   ; espaces autour du crochet
      (success (let1E 'x (numE 5) (varE 'x)) ""))
(test (expP "{let {[x 1]} {let {[y 2]} {+ x y}}}")   ; let imbriqué
      (success (let1E 'x (numE 1) (let1E 'y (numE 2) (plusE (varE 'x) (varE 'y)))) ""))
(test (expP "{let {[x {let {[y 1]} y}]} x}")   ; let dans la valeur liée
      (success (let1E 'x (let1E 'y (numE 1) (varE 'y)) (varE 'x)) ""))
(test (expP "{let {[x 5]} {if true x 0}}")   ; mélange let et if
      (success (let1E 'x (numE 5) (cndE (boolE #t) (varE 'x) (numE 0))) ""))

;; espaces avant le mot-clé
(test (expP "{ let {[x 5]} x}")
      (success (let1E 'x (numE 5) (varE 'x)) ""))
(test (expP "{    let {[x 5]} x}")
      (success (let1E 'x (numE 5) (varE 'x)) ""))

;; ---------- plusieurs expressions de suite ----------
(test ((Many expP) "1 2 3") (success (list (numE 1) (numE 2) (numE 3)) ""))
(test ((Many expP) "{+ 1 2} 3 x")
      (success (list (plusE (numE 1) (numE 2)) (numE 3) (varE 'x)) ""))

;; ---------- rejets ----------
(test (failure? (expP "")) #t)
(test (failure? (expP "{}")) #t)
(test (failure? (expP "{+ 1}")) #t)           ; second opérande manquant
(test (failure? (expP "{+ 1 2")) #t)          ; accolade fermante manquante
(test (failure? (expP "{+ 1 2 3}")) #t)       ; trois opérandes
(test (failure? (expP "{foo 1 2}")) #t)       ; opérateur inconnu
(test (failure? (expP "{if true 1}")) #t)   ; pas de else
(test (failure? (expP "{if {true} 1 2}")) #t)   ; {true} n'est pas une expression
(test (failure? (expP "{let {[x 5]}}")) #t)   ; pas de corps
(test (failure? (expP "}")) #t)

;; ---------- comportements actuels à connaître (à corriger) ----------
;; un mot qui commence par true/false est lu comme un booléen suivi d'un reste
(test (expP "trueish") (success (boolE #t) "ish"))
;;(test (expP "falsey") (success (boolE #f) "ey"))
;; la liaison du let exige au moins un espace après "}" (Many1)
(test (failure? (expP "{let {[x 5]}x}")) #t)


;; ---------- ton exemple, avec la syntaxe de ce parseur ({} et true/false) ----------
(test (expP "{let {[ok {+ 5 6}]} {if ok true false}}")
      (success (let1E 'ok (plusE (numE 5) (numE 6))
                      (cndE (varE 'ok) (boolE #t) (boolE #f)))
               ""))

;; ---------- expressions dans toutes les positions du if ----------
(test (expP "{if {+ 1 5} {+ 2 3} {+ 4 5}}")   ; addition partout
      (success (cndE (plusE (numE 1) (numE 5))
                     (plusE (numE 2) (numE 3))
                     (plusE (numE 4) (numE 5)))
               ""))
(test (expP "{if {if true false true} 1 2}")   ; if dans la condition
      (success (cndE (cndE (boolE #t) (boolE #f) (boolE #t)) (numE 1) (numE 2)) ""))
(test (expP "{if false 1 {if true 2 3}}")      ; if dans le else
      (success (cndE (boolE #f) (numE 1) (cndE (boolE #t) (numE 2) (numE 3))) ""))
(test (expP "{if {let {[x true]} x} 1 2}")     ; let dans la condition
      (success (cndE (let1E 'x (boolE #t) (varE 'x)) (numE 1) (numE 2)) ""))
(test (expP "{if true {let {[x 1]} x} 2}")     ; let dans le then
      (success (cndE (boolE #t) (let1E 'x (numE 1) (varE 'x)) (numE 2)) ""))
(test (expP "{if x {+ x 1} {+ x -1}}")         ; variables et négatifs
      (success (cndE (varE 'x)
                     (plusE (varE 'x) (numE 1))
                     (plusE (varE 'x) (numE -1)))
               ""))

;; ---------- le if comme opérande d'une addition ----------
(test (expP "{+ {if true 1 2} 3}")
      (success (plusE (cndE (boolE #t) (numE 1) (numE 2)) (numE 3)) ""))
(test (expP "{+ {if true 1 2} {if false 3 4}}")
      (success (plusE (cndE (boolE #t) (numE 1) (numE 2))
                      (cndE (boolE #f) (numE 3) (numE 4)))
               ""))

;; ---------- let plus riches ----------
(test (expP "{let {[a 1]} {let {[b {+ a 1}]} {+ a b}}}")   ; b dépend de a
      (success (let1E 'a (numE 1)
                      (let1E 'b (plusE (varE 'a) (numE 1))
                             (plusE (varE 'a) (varE 'b))))
               ""))
(test (expP "{let {[x 1]} {let {[x 2]} x}}")   ; masquage : parse seulement, pas d'évaluation
      (success (let1E 'x (numE 1) (let1E 'x (numE 2) (varE 'x))) ""))
(test (expP "{let {[b true]} b}")
      (success (let1E 'b (boolE #t) (varE 'b)) ""))
(test (expP "{let {[y x]} y}")
      (success (let1E 'y (varE 'x) (varE 'y)) ""))
(test (expP "{let {[long_name_123 5]} long_name_123}")
      (success (let1E 'long_name_123 (numE 5) (varE 'long_name_123)) ""))

;; ---------- espaces partout ----------
(test (expP "{  let  {  [  x  5  ]  }  x  }")
      (success (let1E 'x (numE 5) (varE 'x)) ""))
(test (expP "{  if  x  1  2  }")
      (success (cndE (varE 'x) (numE 1) (numE 2)) ""))
(test (expP "  {if true 1 2}  suite")   ; espaces autour, le reste est conservé
      (success (cndE (boolE #t) (numE 1) (numE 2)) "suite"))

;; ---------- imbrication profonde ----------
(test (expP "{+ 1 {+ 2 {+ 3 {+ 4 {+ 5 6}}}}}")
      (success (plusE (numE 1)
                      (plusE (numE 2)
                             (plusE (numE 3)
                                    (plusE (numE 4) (plusE (numE 5) (numE 6)))))) 
               ""))
(test (expP "{+ -1 -2}") (success (plusE (numE -1) (numE -2)) ""))

;; ---------- mots qui ressemblent à des mots-clés ----------
(test (expP "letter") (success (varE 'letter) ""))   ; let n'est un mot-clé que dans {let ...}
(test (expP "iffy") (success (varE 'iffy) ""))
(test (expP "if") (success (varE 'if) ""))           ; pour l'instant, if seul est une variable

;; ---------- plusieurs expressions de suite, mélangées ----------
(test ((Many expP) "{let {[x 1]} x} {if true 1 2} 3")
      (success (list (let1E 'x (numE 1) (varE 'x))
                     (cndE (boolE #t) (numE 1) (numE 2))
                     (numE 3))
               ""))

;; ---------- rejets supplémentaires ----------
(test (failure? (expP "(+ 5 6)")) #t)            ; parenthèses : pas la syntaxe
(test (failure? (expP "#t")) #t)                  ; #t n'existe pas, c'est true
(test (failure? (expP "{if ok #t #f}")) #t)
(test (failure? (expP "{let {[x]} x}")) #t)       ; liaison sans valeur
(test (failure? (expP "{let {x 5} x}")) #t)       ; pas de crochets
(test (failure? (expP "{let {[x 5] [y 6]} x}")) #t) ; une seule liaison par let
(test (failure? (expP "{if}")) #t)
(test (failure? (expP "{if true}")) #t)
(test (failure? (expP "{+ 1\n2}")) #t)            ; seul l'espace compte comme blanc

;; ---------- encore des comportements actuels à connaître ----------
;; un identifiant peut commencer par un chiffre (pid prend tout id-chars)
(test (expP "{let {[5 1]} 5}")
      (success (let1E (string->symbol "5") (numE 1) (numE 5)) ""))
;; un nombre collé à des lettres : le nombre gagne, le reste n'est pas consommé
(test (expP "1x") (success (numE 1) "x"))