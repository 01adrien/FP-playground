#lang plait

(require "core.rkt"
         "utils.rkt"
         "combinators.rkt")

;; (pchar c)
;; reconnaît exactement le caractère `c` et le consomme ; échoue sur tout autre.
(pchar : (Char -> (Parser Char)))
(define (pchar c)
  (satisfy (lambda (x) (char=? c x))))

;; (pnum)
;; reconnaît un chiffre et renvoie sa valeur numérique.
(pnum : (Parser Number))
(define pnum (satisfy-map digit-value))

;; (pany-char chars)
;; reconnaît n'importe lequel des caractères de la liste `chars`.
(pany-char : ((Listof Char) -> (Parser Char)))
(define (pany-char chars)
  (Any (map pchar chars)))

;; (pdigit)
;; reconnaît un chiffre et renvoie le Char.
(pdigit : (Parser Char))
(define pdigit (pany-char (string->list "0123456789")))

(spaceP : (Parser Char))
(define spaceP (pchar #\space))
