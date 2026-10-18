#lang plait

(require "utils.rkt")

;; Résultat d'un parseur :
;;  - success : la valeur produite + ce qui reste de la chaîne à lire
;;  - failure : un message d'erreur
;; Le 'a est le type de la valeur produite (Char, Number, liste...).
(define-type (ParseResult 'a)
             [success (value : 'a) (remaining : String)]
             [failure (message : String)])

;; Un parseur est une fonction : elle prend une chaîne et renvoie un ParseResult.
;; Construire un parseur (ex: (pchar #\a)) donne une fonction
;; le lancer ((pchar #\a) "abc") donne un résultat.
(define-type-alias (Parser 'a) (String -> (ParseResult 'a)))

;; (lazy-parser parser)
;; Retarde la construction de `parser` jusqu'au moment où on l'utilise.
;; Nécessaire pour les grammaires récursives (E -> ... E ...), car Plait est
;; strict : sans ça, définir E demanderait E avant qu'il existe.
(lazy-parser : ((-> (Parser 'a)) -> (Parser 'a)))
(define (lazy-parser parser)
  (lambda (str) ((parser) str)))

;; (satisfy-map fn)
;; lit un caractère et le passe à `fn`.
;;  - entrée vide        -> échec
;;  - fn return none     -> échec, rien n'est consommé
;;  - fn return (some v) -> succès avec la valeur v, un caractère consommé
(satisfy-map : ((Char -> (Optionof 'a)) -> (Parser 'a)))
(define (satisfy-map fn)
  (lambda (str)
    (type-case (Optionof Char)
               (string-first str)
               [(none) (failure "satisfy(): unexpected end of input")]
               [(some c)
                (type-case (Optionof 'a)
                           (fn c)
                           [(none)
                            (failure (string-append "satisfy(): unexpected " (char->string c)))]
                           [(some v) (success v (string-rest str))])])))

;; (satisfy pred)
;; lit un caractère s'il vérifie `pred`, et le renvoie.
;; Cas particulier de satisfy-map : la valeur produite est le caractère lui-même.
(satisfy : ((Char -> Boolean) -> (Parser Char)))
(define (satisfy pred)
  (satisfy-map (lambda (c)
                 (if (pred c)
                     (some c)
                     (none)))))

;; (bind `parser` fn)
;;  1. lance `parser` sur l'entrée ;
;;  2. si échec, on propage l'échec reconstruit pour changer son type :
;;     un (failure ...) de type 'a n'est pas un (failure ...) de type 'b.
;;  3. si succès, fn reçoit la valeur et fabrique le `next`,
;;     qu'on lance sur le reste de la chaîne.
;; Le parseur suivant peut donc dépendre de la valeur lue juste avant.
(bind : ((Parser 'a) ('a -> (Parser 'b)) -> (Parser 'b)))
(define (bind parser fn)
  (lambda (str)
    (let ([r (parser str)])
      (if (failure? r)
          (failure (failure-message r))
          (let ([next (fn (success-value r))]) (next (success-remaining r)))))))

;; (return val)
;; réussit toujours, sans consommer d'entrée, en produisant `val`.
;; Sert à mettre une valeur dans un parseur .
(return : ('a -> (Parser 'a)))
(define (return val)
  (lambda (str) (success val str)))
