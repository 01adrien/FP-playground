#lang plait

(require "../utils.rkt"
         "../core.rkt"
         "../combinators.rkt"
         "../parsers.rkt")

(define module-name "core")

(print-only-errors #true)

;; Affiche « <module>: <fonction> test OK » à la fin d'une section de tests.
;; Les tests qui échouent s'affichent d'eux-mêmes (print-only-errors), donc cette
;; ligne n'apparaît « dans le vide » que si la section s'est exécutée jusqu'au bout.
(define (done fn-name)
  (display (string-concat (list module-name ": '" fn-name "' test OK\n"))))

;; =====================================================================
;; satisfy-map : lit UN caractère, le passe à une fonction qui répond
;;   (some v) -> succès avec la valeur v, le caractère est consommé
;;   (none)   -> échec
;; =====================================================================

;; fonction qui accepte tout et renvoie toujours 42
(define always-42 (satisfy-map (lambda (_) (some 42))))
;; fonction qui refuse tout (le type est fixé pour éviter une variable libre)
(never : (Parser Number))
(define never (satisfy-map (lambda (_) (none))))
;; fonction qui ne garde que #\a et renvoie 1
(define a-is-1
  (satisfy-map (lambda (c)
                 (if (char=? c #\a)
                     (some 1)
                     (none)))))

;; ---------- succès : la valeur vient de la fonction, pas du caractère ----------
(test (always-42 "abc") (success 42 "bc"))
(test (always-42 "z") (success 42 "")) ; entrée épuisée -> reste vide
(test (always-42 "55") (success 42 "5")) ; un seul caractère consommé
(test (a-is-1 "abc") (success 1 "bc"))

;; ---------- la fonction reçoit bien le PREMIER caractère ----------
(test ((satisfy-map (lambda (c) (some c))) "xyz") (success #\x "yz"))

;; ---------- échec : la fonction refuse ----------
(test (failure? (never "abc")) #t)
(test (failure? (a-is-1 "bac")) #t) ; seul le premier caractère compte
(test (failure? (a-is-1 "xbc")) #t)

;; ---------- échec : entrée vide ----------
(test (failure? (always-42 "")) #t)
(test (failure? (never "")) #t)
(test (failure? (a-is-1 "")) #t)

;; sur l'entrée vide, la fonction ne doit PAS être appelée :
;; si elle l'est, error plante et le test échoue
(define explodes (satisfy-map (lambda (_) (error 'satisfy-map "ne devrait pas être appelée"))))
(test (failure? (explodes "")) #t)

;; ---------- le type du résultat peut changer ----------
;; Char -> String
(test ((satisfy-map (lambda (c) (some (char->string c)))) "xyz") (success "x" "yz"))
;; Char -> Boolean
(test ((satisfy-map (lambda (c) (some (char=? c #\a)))) "abc") (success #t "bc"))
(test ((satisfy-map (lambda (c) (some (char=? c #\a)))) "xbc") (success #f "bc"))

;; ---------- avec digit-value : le parseur de chiffre numérique ----------
(define pdigit-num (satisfy-map digit-value))
(test (pdigit-num "7x") (success 7 "x"))
(test (pdigit-num "0") (success 0 ""))
(test (pdigit-num "9abc") (success 9 "abc"))
(test (failure? (pdigit-num "a7")) #t) ; pas un chiffre
(test (failure? (pdigit-num " 7")) #t)
(test (failure? (pdigit-num "")) #t)
;; caractères voisins de '0' et '9' dans la table ASCII
(test (failure? (pdigit-num "/")) #t)
(test (failure? (pdigit-num ":")) #t)

;; ---------- cohérence avec satisfy ----------
;; satisfy pred doit se comporter comme satisfy-map avec (if (pred c) (some c) (none))
(define (via-map pred)
  (satisfy-map (lambda (c)
                 (if (pred c)
                     (some c)
                     (none)))))
(define is-a (lambda (c) (char=? c #\a)))
;; succès : on compare le résultat entier (valeur ET reste)
(test ((via-map is-a) "abc") ((satisfy is-a) "abc"))
(test ((via-map is-a) "a") ((satisfy is-a) "a"))
;; échec : on ne compare pas le texte du msg, seulement le fait d'échouer
(test (failure? ((via-map is-a) "xbc")) #t)
(test (failure? ((via-map is-a) "")) #t)
(done "satisfy-map")

;; =====================================================================
;; satisfy : lit UN caractère s'il vérifie le prédicat
;; =====================================================================

;; prédicat toujours vrai : accepte n'importe quel premier caractère
(define any-char (satisfy (lambda (c) #t)))
;; prédicat toujours faux : refuse tout
(define no-char (satisfy (lambda (c) #f)))
;; prédicat réel : accepte seulement #\a
(define only-a (satisfy (lambda (c) (char=? c #\a))))

;; succès : renvoie le caractère lu et consomme exactement un caractère
(test (success? (any-char "abc")) #t)
(test (success-val (any-char "abc")) #\a)
(test (success-rem (any-char "abc")) "bc")
(test (success-val (only-a "abc")) #\a)
(test (success-rem (only-a "abc")) "bc")

;; entrée d'un seul caractère : il reste la chaîne vide
(test (success-val (any-char "z")) #\z)
(test (success-rem (any-char "z")) "")

;; le prédicat ne regarde que le PREMIER caractère, pas les suivants
(test (success? (only-a "abb")) #t)
(test (failure? (only-a "baa")) #t)

;; le prédicat reçoit bien le caractère lu (il décide selon sa valeur)
(test (success? ((satisfy (lambda (c) (char=? c #\space))) " x")) #t)
(test (failure? ((satisfy (lambda (c) (char=? c #\space))) "x ")) #t)

;; échec : le prédicat refuse
(test (failure? (no-char "abc")) #t)
(test (failure? (only-a "xbc")) #t)

;; échec : entrée vide, même avec un prédicat toujours vrai
(test (failure? (any-char "")) #t)
(test (failure? (only-a "")) #t)

;; msgs d'erreur (à adapter si tu changes le texte dans core.rkt)
(test (failure-msg (any-char "")) "satisfy(): unexpected end of input")
(test (failure-msg (only-a "xbc")) "satisfy(): unexpected x")

;; satisfy est une valeur réutilisable : le même parseur donne le même résultat
(test (success-val (only-a "abc")) (success-val (only-a "abc")))
(done "satisfy")

;; =====================================================================
;; return : parseur qui réussit TOUJOURS, sans rien consommer
;; =====================================================================
(test (success? ((return 42) "abc")) #t)
(test (success-val ((return 42) "abc")) 42)
(test (success-rem ((return 42) "abc")) "abc") ; la chaîne est rendue intacte
(test (success? ((return 42) "")) #t) ; réussit même sur l'entrée vide
(test (success-rem ((return 42) "")) "")
(test (success-val ((return "x") "abc")) "x") ; marche avec n'importe quel type
(test (success-val ((return #\z) "abc")) #\z)
(done "return")

;; =====================================================================
;; bind : lance un parseur, puis construit le suivant à partir de sa valeur
;; =====================================================================

;; continuation constante : on ignore la valeur lue (ici elle ne sert pas)
(define (run-bind-const input)
  ((bind (pchar #\a) (lambda (_) (pchar #\b))) input))

(test (success? (run-bind-const "abc")) #t)
(test (success-val (run-bind-const "abc")) #\b) ; on obtient la valeur du 2e parseur
(test (success-rem (run-bind-const "abc")) "c")
(test (failure? (run-bind-const "xbc")) #t) ; l'échec du 1er traverse
(test (failure? (run-bind-const "axc")) #t) ; l'échec du 2e aussi
(test (failure? (run-bind-const "a")) #t)
(test (failure? (run-bind-const "")) #t)

;; le second parseur reçoit le RESTE du premier, pas la chaîne d'origine
(test (failure? ((bind (pchar #\a) (lambda (_) (pchar #\a))) "ab")) #t)
(test (success? ((bind (pchar #\a) (lambda (_) (pchar #\a))) "aab")) #t)

;; la continuation dépend de la valeur lue : après 'x' on attend 'y', sinon 'z'
;; (c'est ce que And ne sait PAS faire)
(define (run-bind-dep input)
  ((bind (Or (pchar #\x) (pchar #\w))
         (lambda (c)
           (if (char=? c #\x)
               (pchar #\y)
               (pchar #\z))))
   input))

(test (success-val (run-bind-dep "xy")) #\y)
(test (success-val (run-bind-dep "wz")) #\z)
(test (failure? (run-bind-dep "xz")) #t) ;; x attend y
(test (failure? (run-bind-dep "wy")) #t) ;; w attend z

;; la continuation peut renvoyer return : on transforme la valeur lue
(define (run-bind-return input)
  ((bind pdigit (lambda (c) (return (list c c)))) input))

(test (success-val (run-bind-return "7x")) (list #\7 #\7))
(test (success-rem (run-bind-return "7x")) "x")
(test (failure? (run-bind-return "x7")) #t)

;; chaînage de deux bind (c'est la forme qui redonne And)
(define (run-bind-bind input)
  ((bind (pchar #\a) (lambda (c1) (bind (pchar #\b) (lambda (c2) (return (pair c1 c2)))))) input))

(test (fst (success-val (run-bind-bind "abc"))) #\a)
(test (snd (success-val (run-bind-bind "abc"))) #\b)
(test (success-rem (run-bind-bind "abc")) "c")
(test (failure? (run-bind-bind "axc")) #t)
(test (failure? (run-bind-bind "xbc")) #t)

;; ---------- lois : bind et return doivent être cohérents entre eux ----------

;; identité à gauche : bind (return v) f  se comporte comme  f v
(test (success-val ((bind (return #\a) (lambda (c) (pchar c))) "abc")) #\a)
(test (failure? ((bind (return #\a) (lambda (c) (pchar c))) "xbc")) #t)

;; identité à droite : bind p return  se comporte comme  p
(test (success-val ((bind pdigit return) "5x")) #\5)
(test (success-rem ((bind pdigit return) "5x")) "x")
(test (failure? ((bind pdigit return) "x5")) #t)
(done "bind")
