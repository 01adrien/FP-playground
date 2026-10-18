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
(test (success-value (pdigit "7abc")) #\7) ; la valeur est un Char, pas un Number
(test (success-remaining (pdigit "123")) "23")
(test (success-remaining (pdigit "5")) "")

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
