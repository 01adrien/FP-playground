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
;; reconnaît un seul chiffre et renvoie sa valeur numérique.
(pnum : (Parser Number))
(define pnum (satisfy-map digit-value))

;; (mk-num nums)
;; prend une liste de nombre et accumule *= 10 pour former le nombre final
(mk-num : ((Listof Number) -> Number))
(define (mk-num nums)
  (fst (foldr (lambda (curr acc) (pair (+ (fst acc) (* curr (snd acc))) (* (snd acc) 10)))
              (pair 0 1)
              nums)))

;; (puint)
;; reconnaît un entier positif
(puint : (Parser Number))
(define puint (Map mk-num (Many1 pnum)))

;; (pnegint)
;; reconnaît un entier negatif
(pnegint : (Parser Number))
(define pnegint (Map (lambda (res) (* (snd res) -1)) (And (pchar #\-) puint)))

;; (pint)
;; reconnait un entier negatif ou positif
(pint : (Parser Number))
(define pint (Or puint pnegint))

;; (pany-char chars)
;; reconnaît n'importe lequel des caractères de la liste `chars`.
(pany-char : ((Listof Char) -> (Parser Char)))
(define (pany-char chars)
  (Any (map pchar chars)))

;; (pdigit)
;; reconnaît un chiffre et renvoie le Char.
(pdigit : (Parser Char))
(define pdigit (pany-char (string->list "0123456789")))

;; (spaceP)
;; reconnait un espae
(pspace : (Parser Char))
(define pspace (pchar #\space))

;; (string-chars)
;; caractères acceptés à l'intérieur d'une chaîne littérale "...".
(string-chars : String)
(define string-chars
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 !#$%&'()*+,-./:;<=>?@[]^_`{|}~")

;; (id-chars)
;; caractères acceptés dans un identifiant, style Lisp.
(id-chars : String)
(define id-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/<=>!?_")

;; (pchars-in allowed-chars)
;; reconnaît une ou plusieurs lettres prises dans allowed-chars, renvoie la String.
(pchars-in : (String -> (Parser String)))
(define (pchars-in allowed-chars)
  (Map list->string (Many1 (pany-char (string->list allowed-chars)))))

;; (pid)
;; reconnaît un identifiant.
(pid : (Parser String))
(define pid (pchars-in id-chars))

;; (pstring-literal)
;; reconnaît une chaîne entre guillemets et renvoie son contenu sans les guillemets.
(pstring-literal : (Parser String))
(define pstring-literal (Between (pchar #\") (pchars-in string-chars) (pchar #\")))

(pstring : (String -> (Parser String)))
(define (pstring str)
  (Map list->string (Sequence (map pchar (string->list str)))))

;; (pbool)
;; reconnaît true ou false et renvoie le Boolean correspondant.
(pbool : (Parser Boolean))
(define pbool (Or (Replace (pstring "true") #t) (Replace (pstring "false") #f)))
