#lang plait

(require "../utils.rkt")

(define module-name "utils")

(print-only-errors #true)

;; Affiche « <module>: <fonction> test OK » à la fin d'une section de tests.
(define (done fn-name)
  (display (string-concat (list module-name ": '" fn-name "' test OK\n"))))

;; =====================================================================
;; UTILS : fonctions auxiliaires sur les chaînes et les caractères
;; =====================================================================

;; ---------- empty-string? ----------
(test (empty-string? "") #true)
(test (empty-string? "ddd") #false)
(test (empty-string? " ") #false) ; un espace n'est pas la chaîne vide
(done "empty-string?")

;; ---------- string-concat ----------
;; colle une liste de chaînes ; liste vide -> chaîne vide
(test (string-concat (list "a" "b" "c")) "abc")
(test (string-concat (list "abc")) "abc") ; un seul élément
(test (string-concat (list "ab" "" "cd")) "abcd") ; une chaîne vide au milieu
(test (string-concat empty) "")
(done "string-concat")

;; ---------- char->string ----------
;; un Char devient une String d'un seul caractère
(test (char->string #\a) "a")
(test (char->string #\G) "G")
(test (char->string #\space) " ")
(done "char->string")

;; ---------- string-rest ----------
;; tout sauf le premier caractère
(test (string-rest "abc") "bc")
(test (string-rest "ab") "b")
(test (string-rest "a") "")
(test (string-rest " x") "x")
(done "string-rest")

;; ---------- string-first ----------
;; le premier caractère
(test (string-first "abc") (some #\a))
(test (string-first "a") (some #\a))
(test (string-first " x") (some #\space))
(test (string-first "7up") (some #\7))
(test (string-first "") (none))
(done "string-first")

;; ---------- digit-value ----------
;; valeur numérique d'un chiffre
(test (digit-value #\0) (some 0))
(test (digit-value #\1) (some 1))
(test (digit-value #\5) (some 5))
(test (digit-value #\9) (some 9))
(test (digit-value #\T) (none))
(done "digit-value")
