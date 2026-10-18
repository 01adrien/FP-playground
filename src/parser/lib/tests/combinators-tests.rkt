#lang plait

;; Adapte ces require à ton arborescence : And/Or/Map/Triple viennent de
;; combinators.rkt, pchar/pdigit de core.rkt (ou parsers.rkt chez toi), et
;; success/failure du module qui définit ParseResult.
(require "../utils.rkt"
         "../core.rkt"
         "../combinators.rkt"
         "../parsers.rkt")

(define module-name "combinators")

;; N'affiche que les tests qui échouent : le silence veut dire que tout passe.
(print-only-errors #true)

;; Affiche « <module>: <fonction> test OK » à la fin d'une section de tests.
(define (done fn-name)
  (display (string-concat (list module-name ": '" fn-name "' test OK\n"))))

;; =====================================================================
;; And : enchaîne deux parseurs, la valeur est une PAIRE (a . b)
;; Le second parseur reçoit le RESTE du premier.
;; =====================================================================

;; chiffre puis lettre 'a'
(define (run-d-a input)
  ((And pdigit (pchar #\a)) input))
(test (success? (run-d-a "7abc")) #t)
(test (fst (success-value (run-d-a "7abc"))) #\7) ; fst = valeur du 1er parseur
(test (snd (success-value (run-d-a "7abc"))) #\a) ; snd = valeur du 2e parseur
(test (success-remaining (run-d-a "7abc")) "bc")
(test (failure? (run-d-a "xabc")) #t) ; le premier échoue
(test (failure? (run-d-a "7xbc")) #t) ; le second échoue
(test (failure? (run-d-a "7")) #t) ; plus rien pour le second

;; lettre puis chiffre
(define (run-a-d input)
  ((And (pchar #\a) pdigit) input))
(test (success? (run-a-d "a5")) #t)
(test (snd (success-value (run-a-d "a5"))) #\5)
(test (failure? (run-a-d "aa")) #t)
(test (failure? (run-a-d "5a")) #t) ; ordre inversé : échec

;; deux chiffres : prouve que le second reçoit le reste du premier
(define (run-dd input)
  ((And pdigit pdigit) input))
(test (success? (run-dd "12x")) #t)
(test (fst (success-value (run-dd "12x"))) #\1)
(test (snd (success-value (run-dd "12x"))) #\2)
(test (success-remaining (run-dd "12x")) "x")
(test (failure? (run-dd "1")) #t)
(test (failure? (run-dd "1a")) #t)

;; trois chiffres : And imbriqué à GAUCHE -> ((d1 . d2) . d3)
(define (run-ddd input)
  ((And (And pdigit pdigit) pdigit) input))
(test (success? (run-ddd "123!")) #t)
(test (fst (fst (success-value (run-ddd "123!")))) #\1)
(test (snd (fst (success-value (run-ddd "123!")))) #\2)
(test (snd (success-value (run-ddd "123!"))) #\3)
(test (success-remaining (run-ddd "123!")) "!")
(test (failure? (run-ddd "12a")) #t)
(done "And")

;; =====================================================================
;; Or : essaie le premier parseur, sinon le second
;; Les deux doivent produire le même type de valeur.
;; =====================================================================

;; 'a' ou 'b'
(define (run-a-or-b input)
  ((Or (pchar #\a) (pchar #\b)) input))
(test (success? (run-a-or-b "abc")) #t) ; le premier réussit
(test (success-value (run-a-or-b "abc")) #\a)
(test (success-remaining (run-a-or-b "abc")) "bc")
(test (success? (run-a-or-b "bcd")) #t) ; le premier échoue, le second réussit
(test (success-value (run-a-or-b "bcd")) #\b)
(test (success-remaining (run-a-or-b "bcd")) "cd")
(test (failure? (run-a-or-b "xyz")) #t) ; les deux échouent
(test (failure? (run-a-or-b "")) #t)
(test (success-remaining (run-a-or-b "b")) "")

;; chiffre ou 'a'
(define (run-d-or-a input)
  ((Or pdigit (pchar #\a)) input))
(test (success-value (run-d-or-a "5x")) #\5)
(test (success-value (run-d-or-a "ax")) #\a)
(test (success-remaining (run-d-or-a "5x")) "x")
(test (success-remaining (run-d-or-a "ax")) "x")
(test (failure? (run-d-or-a "x5")) #t)

;; 'a' ou ('b' ou 'c') : le choix s'imbrique
(define (run-abc-Any input)
  ((Or (pchar #\a) (Or (pchar #\b) (pchar #\c))) input))
(test (success-value (run-abc-Any "a")) #\a)
(test (success-value (run-abc-Any "b")) #\b)
(test (success-value (run-abc-Any "c")) #\c)
(test (failure? (run-abc-Any "d")) #t)

;; "ab" ou "cd" : Or combiné avec And
(define (run-ab-or-cd input)
  ((Or (And (pchar #\a) (pchar #\b)) (And (pchar #\c) (pchar #\d))) input))
(test (fst (success-value (run-ab-or-cd "abz"))) #\a)
(test (snd (success-value (run-ab-or-cd "abz"))) #\b)
(test (fst (success-value (run-ab-or-cd "cdz"))) #\c)
(test (snd (success-value (run-ab-or-cd "cdz"))) #\d)
(test (success-remaining (run-ab-or-cd "cdz")) "z")
(test (failure? (run-ab-or-cd "xyz")) #t)

;; Test du « retour en arrière » : après un échec PARTIEL du premier choix
;; ("a" lu puis 'b' attendu), le second choix repart de la chaîne d'ORIGINE.
(test (failure? (run-ab-or-cd "ac")) #t) ; ni "ab" ni "cd"
(test (success? ((Or (And (pchar #\a) (pchar #\b)) (And (pchar #\a) (pchar #\c))) "ac"))
      #t) ; "ac" réussit via le second choix
(done "Or")

;; =====================================================================
;; Map : transforme la valeur d'un succès, sans toucher au reste
;; L'échec traverse sans appeler la fonction.
;; =====================================================================

;; ---------- Map : cas de base ----------

;; transformer la valeur : Char -> String
(define (run-digit-str input)
  ((Map char->string pdigit) input))

(test (success? (run-digit-str "7x")) #t)
(test (success-value (run-digit-str "7x")) "7") ; #\7 est devenu "7"
(test (success-remaining (run-digit-str "7x")) "x") ; le reste est inchangé

;; l'échec traverse sans appeler la fonction
(test (failure? (run-digit-str "x7")) #t)
(test (failure? (run-digit-str "")) #t)

;; changer de type : Char -> Boolean
(define (run-is-a input)
  ((Map (lambda (c) (char=? c #\a)) (pchar #\a)) input))

(test (success-value (run-is-a "abc")) #t)
(test (success-remaining (run-is-a "abc")) "bc")
(test (failure? (run-is-a "bcd")) #t)

;; une fonction constante ignore la valeur lue
(define (run-const input)
  ((Map (lambda (_) 42) (pchar #\a)) input))

(test (success-value (run-const "abc")) 42)
(test (success-remaining (run-const "abc")) "bc")
(test (failure? (run-const "xbc")) #t)

;; ---------- Map avec And ----------

;; paire de Char -> String de deux caractères
(define (pair->string p)
  (string-append (char->string (fst p)) (char->string (snd p))))

(define (run-two-digits input)
  ((Map pair->string (And pdigit pdigit)) input))

(test (success-value (run-two-digits "42x")) "42")
(test (success-remaining (run-two-digits "42x")) "x")
(test (failure? (run-two-digits "4x")) #t)
(test (failure? (run-two-digits "x4")) #t)

;; Map n'avale pas d'entrée supplémentaire
(test (success-remaining (run-two-digits "42")) "")

;; ---------- Map composé ----------

;; deux Map à la suite : Char -> String -> longueur
(define (run-compose input)
  ((Map string-length (Map char->string pdigit)) input))

(test (success-value (run-compose "5x")) 1)
(test (success-remaining (run-compose "5x")) "x")
(test (failure? (run-compose "x5")) #t)

;; ---------- lois (comme pour bind) ----------

;; identité : Map avec la fonction identité ne change rien
(define (id x)
  x)
(test (success-value ((Map id pdigit) "5x")) #\5)
(test (success-remaining ((Map id pdigit) "5x")) "x")
(test (failure? ((Map id pdigit) "x5")) #t)

;; composition : Map (f o g) = Map f . Map g
(define (f s)
  (string-append s "!"))
(define (g c)
  (char->string c))
(define (f-after-g c)
  (f (g c)))

(test (success-value ((Map f-after-g pdigit) "5x")) (success-value ((Map f (Map g pdigit)) "5x")))
(test (success-remaining ((Map f-after-g pdigit) "5x"))
      (success-remaining ((Map f (Map g pdigit)) "5x")))
(done "Map")

;; =====================================================================
;; Any : essaie une liste de parseurs dans l'ordre, le premier qui
;; réussit gagne. Liste vide -> échec.
;; =====================================================================

;; liste vide : rien à essayer
(empty-Any : (Parser Char))
(define empty-Any (Any empty))
(test (failure? (empty-Any "abc")) #t)

;; chaque position peut gagner : premier, milieu, dernier
(define abc (Any (list (pchar #\a) (pchar #\b) (pchar #\c))))
(test (abc "a!") (success #\a "!"))
(test (abc "b!") (success #\b "!"))
(test (abc "c") (success #\c ""))
(test (failure? (abc "d!")) #t) ; aucun ne réussit
(test (failure? (abc "")) #t) ; entrée vide

;; l'ordre compte : quand deux parseurs réussiraient, le premier gagne
(define first-wins
  (Any (list (Map (lambda (_) "premier") pdigit) (Map (lambda (_) "second") pdigit))))
(test (first-wins "5x") (success "premier" "x"))

;; retour en arrière : un échec PARTIEL du premier ne consomme rien
(define ab (Map pair->string (And (pchar #\a) (pchar #\b))))
(define ac (Map pair->string (And (pchar #\a) (pchar #\c))))
(define ab-or-ac (Any (list ab ac)))
(test (ab-or-ac "abz") (success "ab" "z"))
(test (ab-or-ac "acz") (success "ac" "z")) ; repart de la chaîne d'origine
(test (failure? (ab-or-ac "adz")) #t)

;; court-circuit : après un succès, les parseurs suivants ne sont pas lancés
(boom : (Parser Char))
(define boom (lambda (str) (error 'boom "ne devrait pas être appelé")))
(test ((Any (list (pchar #\a) boom)) "abc") (success #\a "bc"))

;; liste plus longue : tous les chiffres
(define any-digit (Any (map pchar (string->list "0123456789"))))
(test (any-digit "5z") (success #\5 "z"))
(test (failure? (any-digit "/")) #t) ; juste avant '0'
(test (failure? (any-digit ":")) #t) ; juste après '9'
(done "Any")

;; =====================================================================
;; Triple : trois parseurs à la suite
;; La valeur est une paire imbriquée À DROITE : (a . (b . c))
;; Les accesseurs ci-dessous évitent de répéter fst/snd partout.
;; =====================================================================

;; accès aux trois éléments d'une valeur ('a * ('b * 'c))
(define (t1 v)
  (fst v)) ; premier élément
(define (t2 v)
  (fst (snd v))) ; deuxième élément
(define (t3 v)
  (snd (snd v))) ; troisième élément

(define (run-abc input)
  ((Triple (pchar #\a) (pchar #\b) (pchar #\c)) input))

;; succès : les trois valeurs, dans l'ordre des parseurs, et le reste
(test (success? (run-abc "abcd")) #t)
(test (t1 (success-value (run-abc "abcd"))) #\a)
(test (t2 (success-value (run-abc "abcd"))) #\b)
(test (t3 (success-value (run-abc "abcd"))) #\c)
(test (success-remaining (run-abc "abcd")) "d")
(test (success-remaining (run-abc "abc")) "")

;; échec à chaque position
(test (failure? (run-abc "xbcd")) #t) ;; premier
(test (failure? (run-abc "axcd")) #t) ;; deuxième
(test (failure? (run-abc "abxd")) #t) ;; troisième
(test (failure? (run-abc "ab")) #t) ;; entrée trop courte
(test (failure? (run-abc "")) #t)

;; chaque parseur reçoit le reste du précédent
(define (run-aaa input)
  ((Triple (pchar #\a) (pchar #\a) (pchar #\a)) input))

(test (success? (run-aaa "aaab")) #t)
(test (success-remaining (run-aaa "aaab")) "b")
(test (failure? (run-aaa "aab")) #t)
(test (failure? (run-aaa "aa")) #t)

;; avec des chiffres
(define (run-3d input)
  ((Triple pdigit pdigit pdigit) input))

(test (t1 (success-value (run-3d "123!"))) #\1)
(test (t2 (success-value (run-3d "123!"))) #\2)
(test (t3 (success-value (run-3d "123!"))) #\3)
(test (success-remaining (run-3d "123!")) "!")
(test (failure? (run-3d "12a")) #t)
(test (failure? (run-3d "a12")) #t)

;; types différents : Number, Char, Char (c'est l'intérêt d'un vrai triplet)
;; (digit-value vient de utils.rkt)
(define (run-mixed input)
  ((Triple (Map digit-value pdigit) (pchar #\,) pdigit) input))

(test (success? (run-mixed "7,8x")) #t)
(test (t1 (success-value (run-mixed "7,8x"))) (some 7)) ; un Number
(test (t2 (success-value (run-mixed "7,8x"))) #\,) ; un Char
(test (t3 (success-value (run-mixed "7,8x"))) #\8) ; un Char
(test (success-remaining (run-mixed "7,8x")) "x")
(test (failure? (run-mixed "7;8")) #t)
(test (failure? (run-mixed "7,x")) #t)

;; l'ordre des éléments suit l'ordre des parseurs
(define (run-order input)
  ((Triple (pchar #\x) pdigit (pchar #\z)) input))

(test (t1 (success-value (run-order "x5z"))) #\x)
(test (t2 (success-value (run-order "x5z"))) #\5)
(test (t3 (success-value (run-order "x5z"))) #\z)

;; cohérence avec And imbriqué à droite : (a . (b . c))
;; Triple doit se comporter comme And a (And b c)
(define (run-nested input)
  ((And (pchar #\a) (And (pchar #\b) (pchar #\c))) input))

(test (success? (run-abc "abcd")) (success? (run-nested "abcd")))
(test (failure? (run-abc "abxd")) (failure? (run-nested "abxd")))
(test (success-remaining (run-abc "abcd")) (success-remaining (run-nested "abcd")))
(test (t1 (success-value (run-abc "abcd"))) (fst (success-value (run-nested "abcd"))))
(test (t3 (success-value (run-abc "abcd"))) (snd (snd (success-value (run-nested "abcd")))))
(done "Triple")

;; =====================================================================
;; SEQUENCE : enchaîne une liste de parseurs, collecte les valeurs
;; =====================================================================

;; petit parseur local pour ne pas dépendre de parsers.rkt
(define (ch [c : Char])
  (satisfy (lambda (x) (char=? x c))))

;; ---------- cas de base : liste vide ----------
;; réussit toujours, valeur = liste vide, rien n'est consommé
(test ((Sequence empty) "abc") (success empty "abc"))
(test ((Sequence empty) "") (success empty ""))

;; ---------- un seul parseur ----------
(test ((Sequence (list (ch #\a))) "abc") (success (list #\a) "bc"))
(test (failure? ((Sequence (list (ch #\a))) "xbc")) #t)

;; ---------- plusieurs parseurs ----------
;; les valeurs sortent dans l'ordre des parseurs
(test ((Sequence (list (ch #\a) (ch #\b) (ch #\c))) "abcd") (success (list #\a #\b #\c) "d"))
;; consomme exactement l'entrée
(test ((Sequence (list (ch #\a) (ch #\b))) "ab") (success (list #\a #\b) ""))
;; même parseur répété
(test ((Sequence (list (ch #\a) (ch #\a) (ch #\a))) "aaab") (success (list #\a #\a #\a) "b"))

;; ---------- l'ordre compte ----------
(test (failure? ((Sequence (list (ch #\b) (ch #\a))) "ab")) #t)

;; ---------- échecs : à n'importe quelle position ----------
(test (failure? ((Sequence (list (ch #\a) (ch #\b) (ch #\c))) "xbc")) #t) ; premier
(test (failure? ((Sequence (list (ch #\a) (ch #\b) (ch #\c))) "axc")) #t) ; milieu
(test (failure? ((Sequence (list (ch #\a) (ch #\b) (ch #\c))) "abx")) #t) ; dernier
;; entrée trop courte
(test (failure? ((Sequence (list (ch #\a) (ch #\b))) "a")) #t)
(test (failure? ((Sequence (list (ch #\a) (ch #\b))) "")) #t)

;; return ne consomme rien et ajoute sa valeur
(test ((Sequence (list (ch #\a) (return #\z) (ch #\b))) "abc") (success (list #\a #\z #\b) "c"))

;; ---------- composition avec Map ----------
;; la séquence est elle-même un parseur : on peut la transformer
(test ((Map list->string (Sequence (list (ch #\h) (ch #\i)))) "hi!") (success "hi" "!"))

;; ---------- avec Any à l'intérieur ----------
(test ((Sequence (list (Any (list (ch #\a) (ch #\b))) (ch #\c))) "bcd") (success (list #\b #\c) "d"))
(test (failure? ((Sequence (list (Any (list (ch #\a) (ch #\b))) (ch #\c))) "xcd")) #t)

(done "Sequence")
