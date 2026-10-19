#lang plait

(require "../utils.rkt"
         "../core.rkt"
         "../combinators.rkt"
         "../parsers.rkt")

(define module-name "parsers")

(print-only-errors #true)

;; Affiche « <module>: <fonction> test OK » à la fin d'une section de tests.
(define (done fn-name)
  (display (string-concat (list module-name ": '" fn-name "' test OK\n"))))

;; =====================================================================
;; PCHAR : reconnaît UN caractère précis
;; Ici on compare directement la valeur du résultat (success valeur reste).
;; =====================================================================

;; cas de succès : le caractère est consommé, le reste est rendu intact
(test ((pchar #\a) "abc") (success #\a "bc"))
(test ((pchar #\a) "a") (success #\a "")) ; entrée épuisée -> reste vide
(test ((pchar #\a) "aaa") (success #\a "aa")) ; un seul caractère consommé
(test ((pchar #\1) "123") (success #\1 "23"))
(test ((pchar #\space) " x") (success #\space "x"))

;; cas d'échec : mauvais caractère, entrée vide, casse différente
(test (failure? ((pchar #\a) "bcd")) #t)
(test (failure? ((pchar #\a) "")) #t)
(test (failure? ((pchar #\a) "Abc")) #t) ; 'A' n'est pas 'a'
(test (failure? ((pchar #\a) "ba")) #t) ; seul le début compte
(done "pchar")

;; =====================================================================
;; PDIGIT : reconnaît UN chiffre (0 à 9)
;; pdigit est un parseur (une valeur), pas une fonction : pas de paramètre.
;; =====================================================================

(test (success? (pdigit "0")) #t) ; bornes de l'intervalle
(test (success? (pdigit "9")) #t)
(test (success-val (pdigit "7abc")) #\7) ; la valeur est un Char, pas un Number
(test (success-rem (pdigit "123")) "23")
(test (success-rem (pdigit "5")) "")

;; rejets : lettre, espace, signe, vide
(test (failure? (pdigit "a1")) #t)
(test (failure? (pdigit " 1")) #t)
(test (failure? (pdigit "-1")) #t)
(test (failure? (pdigit "")) #t)
;; caractères voisins dans la table ASCII : attrapent un < à la place d'un <=
(test (failure? (pdigit "/")) #t) ; juste avant '0'
(test (failure? (pdigit ":")) #t) ; juste après '9'
(done "pdigit")

;; =====================================================================
;; pany-char : lit UN caractère qui fait partie de la liste donnée.
;; Liste vide -> échec. La valeur produite est le caractère lu.
;; =====================================================================

;; liste vide : aucun caractère n'est acceptable
(any-nothing : (Parser Char))
(define any-nothing (pany-char empty))
(test (failure? (any-nothing "abc")) #t)

;; un seul caractère dans la liste : se comporte comme pchar
(define any-a (pany-char (list #\a)))
(test (any-a "abc") (success #\a "bc"))
(test (failure? (any-a "xbc")) #t)

;; plusieurs caractères : chacun est accepté, quelle que soit sa position
(define any-abc (pany-char (list #\a #\b #\c)))
(test (any-abc "a!") (success #\a "!")) ; premier de la liste
(test (any-abc "b!") (success #\b "!")) ; milieu
(test (any-abc "c") (success #\c "")) ; dernier, entrée épuisée -> reste vide
(test (failure? (any-abc "d!")) #t) ; absent de la liste
(test (failure? (any-abc "")) #t) ; entrée vide

;; un seul caractère consommé, même si les suivants sont aussi dans la liste
(test (any-abc "abc") (success #\a "bc"))

;; l'ordre dans la liste n'a pas d'importance
(test ((pany-char (list #\c #\b #\a)) "bz") (success #\b "z"))

;; seul le premier caractère de l'entrée compte
(test (failure? (any-abc "xab")) #t)

;; la casse compte, et l'espace est un caractère comme un autre
(test (failure? (any-abc "A")) #t)
(test ((pany-char (list #\space #\tab)) " x") (success #\space "x"))

;; doublons dans la liste : sans effet
(test ((pany-char (list #\a #\a #\b)) "bz") (success #\b "z"))

;; équivalent de pdigit : tous les chiffres, et pas leurs voisins ASCII
(define any-digit-char (pany-char (string->list "0123456789")))
(test (any-digit-char "0z") (success #\0 "z"))
(test (any-digit-char "9z") (success #\9 "z"))
(test (failure? (any-digit-char "/")) #t) ; juste avant '0'
(test (failure? (any-digit-char ":")) #t) ; juste après '9'
(test (any-digit-char "5z") (pdigit "5z")) ; même résultat que pdigit
(done "pany-char")

;; =====================================================================
;; SPACEP : reconnaît UN espace (le caractère #\space)
;; =====================================================================

;; ---------- succès : le caractère et le reste ----------
(test (spaceP " abc") (success #\space "abc"))
(test (spaceP " ") (success #\space ""))

;; ---------- un seul espace est consommé ----------
(test (spaceP "  x") (success #\space " x"))
(test (spaceP "   ") (success #\space "  "))

;; ---------- rejets ----------
(test (failure? (spaceP "abc")) #t)
(test (failure? (spaceP "")) #t)
(test (failure? (spaceP "1 ")) #t) ; l'espace n'est pas en tête
;; les autres blancs ne sont PAS des #\space
(test (failure? (spaceP "\t")) #t)
(test (failure? (spaceP "\n")) #t)

;; ---------- avec Many : le cas qui t'intéresse vraiment ----------
(test ((Many spaceP) "   x") (success (list #\space #\space #\space) "x"))
(test ((Many spaceP) "x") (success empty "x")) ; zéro espace : succès, pas échec
(test ((Many spaceP) "") (success empty ""))

;; =====================================================================
;; PNUM : reconnaît UN chiffre et renvoie sa valeur numérique
;; =====================================================================

;; ---------- chaque chiffre donne sa valeur (un Number, pas un Char) ----------
(test (success-val (pnum "0")) 0)
(test (success-val (pnum "1")) 1)
(test (success-val (pnum "2")) 2)
(test (success-val (pnum "3")) 3)
(test (success-val (pnum "4")) 4)
(test (success-val (pnum "5")) 5)
(test (success-val (pnum "6")) 6)
(test (success-val (pnum "7")) 7)
(test (success-val (pnum "8")) 8)
(test (success-val (pnum "9")) 9)

;; ---------- le résultat complet : valeur et reste ----------
(test (pnum "7abc") (success 7 "abc"))
(test (pnum "5") (success 5 ""))

;; ---------- un seul chiffre est consommé ----------
(test (pnum "123") (success 1 "23")) ; pas 123
(test (pnum "99") (success 9 "9"))

;; ---------- rejets ----------
(test (failure? (pnum "a1")) #t)
(test (failure? (pnum " 1")) #t) ; espace devant
(test (failure? (pnum "-1")) #t) ; pas de signe
(test (failure? (pnum "")) #t)
;; caractères voisins dans la table ASCII : attrapent un < à la place d'un <=
(test (failure? (pnum "/")) #t) ; juste avant '0'
(test (failure? (pnum ":")) #t) ; juste après '9'

;; ---------- cohérence avec pdigit ----------
;; pnum doit voir exactement les mêmes chiffres que pdigit
(test (success? (pnum "4")) (success? (pdigit "4")))
(test (failure? (pnum "x")) (failure? (pdigit "x")))
