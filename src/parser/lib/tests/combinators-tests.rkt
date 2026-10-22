#lang plait

;; Adapte ces require à ton arborescence : And/Or/Map/Triple viennent de
;; combinators.rkt, ch/pdigit de core.rkt (ou parsers.rkt chez toi), et
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

(define (ch [c : Char])
  (satisfy (lambda (x) (char=? x c))))

;; =====================================================================
;; And : enchaîne deux parseurs, la valeur est une PAIRE (a . b)
;; Le second parseur reçoit le RESTE du premier.
;; =====================================================================

;; chiffre puis lettre 'a'
(define (run-d-a input)
  ((And pdigit (ch #\a)) input))
(test (success? (run-d-a "7abc")) #t)
(test (fst (success-val (run-d-a "7abc"))) #\7) ; fst = valeur du 1er parseur
(test (snd (success-val (run-d-a "7abc"))) #\a) ; snd = valeur du 2e parseur
(test (success-rem (run-d-a "7abc")) "bc")
(test (failure? (run-d-a "xabc")) #t) ; le premier échoue
(test (failure? (run-d-a "7xbc")) #t) ; le second échoue
(test (failure? (run-d-a "7")) #t) ; plus rien pour le second

;; lettre puis chiffre
(define (run-a-d input)
  ((And (ch #\a) pdigit) input))
(test (success? (run-a-d "a5")) #t)
(test (snd (success-val (run-a-d "a5"))) #\5)
(test (failure? (run-a-d "aa")) #t)
(test (failure? (run-a-d "5a")) #t) ; ordre inversé : échec

;; deux chiffres : prouve que le second reçoit le reste du premier
(define (run-dd input)
  ((And pdigit pdigit) input))
(test (success? (run-dd "12x")) #t)
(test (fst (success-val (run-dd "12x"))) #\1)
(test (snd (success-val (run-dd "12x"))) #\2)
(test (success-rem (run-dd "12x")) "x")
(test (failure? (run-dd "1")) #t)
(test (failure? (run-dd "1a")) #t)

;; trois chiffres : And imbriqué à GAUCHE -> ((d1 . d2) . d3)
(define (run-ddd input)
  ((And (And pdigit pdigit) pdigit) input))
(test (success? (run-ddd "123!")) #t)
(test (fst (fst (success-val (run-ddd "123!")))) #\1)
(test (snd (fst (success-val (run-ddd "123!")))) #\2)
(test (snd (success-val (run-ddd "123!"))) #\3)
(test (success-rem (run-ddd "123!")) "!")
(test (failure? (run-ddd "12a")) #t)
(done "And")

;; =====================================================================
;; Or : essaie le premier parseur, sinon le second
;; Les deux doivent produire le même type de valeur.
;; =====================================================================

;; 'a' ou 'b'
(define (run-a-or-b input)
  ((Or (ch #\a) (ch #\b)) input))
(test (success? (run-a-or-b "abc")) #t) ; le premier réussit
(test (success-val (run-a-or-b "abc")) #\a)
(test (success-rem (run-a-or-b "abc")) "bc")
(test (success? (run-a-or-b "bcd")) #t) ; le premier échoue, le second réussit
(test (success-val (run-a-or-b "bcd")) #\b)
(test (success-rem (run-a-or-b "bcd")) "cd")
(test (failure? (run-a-or-b "xyz")) #t) ; les deux échouent
(test (failure? (run-a-or-b "")) #t)
(test (success-rem (run-a-or-b "b")) "")

;; chiffre ou 'a'
(define (run-d-or-a input)
  ((Or pdigit (ch #\a)) input))
(test (success-val (run-d-or-a "5x")) #\5)
(test (success-val (run-d-or-a "ax")) #\a)
(test (success-rem (run-d-or-a "5x")) "x")
(test (success-rem (run-d-or-a "ax")) "x")
(test (failure? (run-d-or-a "x5")) #t)

;; 'a' ou ('b' ou 'c') : le choix s'imbrique
(define (run-abc-Any input)
  ((Or (ch #\a) (Or (ch #\b) (ch #\c))) input))
(test (success-val (run-abc-Any "a")) #\a)
(test (success-val (run-abc-Any "b")) #\b)
(test (success-val (run-abc-Any "c")) #\c)
(test (failure? (run-abc-Any "d")) #t)

;; "ab" ou "cd" : Or combiné avec And
(define (run-ab-or-cd input)
  ((Or (And (ch #\a) (ch #\b)) (And (ch #\c) (ch #\d))) input))
(test (fst (success-val (run-ab-or-cd "abz"))) #\a)
(test (snd (success-val (run-ab-or-cd "abz"))) #\b)
(test (fst (success-val (run-ab-or-cd "cdz"))) #\c)
(test (snd (success-val (run-ab-or-cd "cdz"))) #\d)
(test (success-rem (run-ab-or-cd "cdz")) "z")
(test (failure? (run-ab-or-cd "xyz")) #t)

;; Test du « retour en arrière » : après un échec PARTIEL du premier choix
;; ("a" lu puis 'b' attendu), le second choix repart de la chaîne d'ORIGINE.
(test (failure? (run-ab-or-cd "ac")) #t) ; ni "ab" ni "cd"
(test (success? ((Or (And (ch #\a) (ch #\b)) (And (ch #\a) (ch #\c))) "ac"))
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
(test (success-val (run-digit-str "7x")) "7") ; #\7 est devenu "7"
(test (success-rem (run-digit-str "7x")) "x") ; le reste est inchangé

;; l'échec traverse sans appeler la fonction
(test (failure? (run-digit-str "x7")) #t)
(test (failure? (run-digit-str "")) #t)

;; changer de type : Char -> Boolean
(define (run-is-a input)
  ((Map (lambda (c) (char=? c #\a)) (ch #\a)) input))

(test (success-val (run-is-a "abc")) #t)
(test (success-rem (run-is-a "abc")) "bc")
(test (failure? (run-is-a "bcd")) #t)

;; une fonction constante ignore la valeur lue
(define (run-const input)
  ((Map (lambda (_) 42) (ch #\a)) input))

(test (success-val (run-const "abc")) 42)
(test (success-rem (run-const "abc")) "bc")
(test (failure? (run-const "xbc")) #t)

;; ---------- Map avec And ----------

;; paire de Char -> String de deux caractères
(define (pair->string p)
  (string-append (char->string (fst p)) (char->string (snd p))))

(define (run-two-digits input)
  ((Map pair->string (And pdigit pdigit)) input))

(test (success-val (run-two-digits "42x")) "42")
(test (success-rem (run-two-digits "42x")) "x")
(test (failure? (run-two-digits "4x")) #t)
(test (failure? (run-two-digits "x4")) #t)

;; Map n'avale pas d'entrée supplémentaire
(test (success-rem (run-two-digits "42")) "")

;; ---------- Map composé ----------

;; deux Map à la suite : Char -> String -> longueur
(define (run-compose input)
  ((Map string-length (Map char->string pdigit)) input))

(test (success-val (run-compose "5x")) 1)
(test (success-rem (run-compose "5x")) "x")
(test (failure? (run-compose "x5")) #t)

;; ---------- lois (comme pour bind) ----------

;; identité : Map avec la fonction identité ne change rien
(define (id x) x)
(test (success-val ((Map id pdigit) "5x")) #\5)
(test (success-rem ((Map id pdigit) "5x")) "x")
(test (failure? ((Map id pdigit) "x5")) #t)

;; composition : Map (f o g) = Map f . Map g
(define (f s)
  (string-append s "!"))
(define (g c)
  (char->string c))
(define (f-after-g c)
  (f (g c)))

(test (success-val ((Map f-after-g pdigit) "5x")) (success-val ((Map f (Map g pdigit)) "5x")))
(test (success-rem ((Map f-after-g pdigit) "5x")) (success-rem ((Map f (Map g pdigit)) "5x")))
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
(define abc (Any (list (ch #\a) (ch #\b) (ch #\c))))
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
(define ab (Map pair->string (And (ch #\a) (ch #\b))))
(define ac (Map pair->string (And (ch #\a) (ch #\c))))
(define ab-or-ac (Any (list ab ac)))
(test (ab-or-ac "abz") (success "ab" "z"))
(test (ab-or-ac "acz") (success "ac" "z")) ; repart de la chaîne d'origine
(test (failure? (ab-or-ac "adz")) #t)

;; court-circuit : après un succès, les parseurs suivants ne sont pas lancés
(boom : (Parser Char))
(define boom (lambda (str) (error 'boom "ne devrait pas être appelé")))
(test ((Any (list (ch #\a) boom)) "abc") (success #\a "bc"))

;; liste plus longue : tous les chiffres
(define any-digit (Any (map ch (string->list "0123456789"))))
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
  ((Triple (ch #\a) (ch #\b) (ch #\c)) input))

;; succès : les trois valeurs, dans l'ordre des parseurs, et le reste
(test (success? (run-abc "abcd")) #t)
(test (t1 (success-val (run-abc "abcd"))) #\a)
(test (t2 (success-val (run-abc "abcd"))) #\b)
(test (t3 (success-val (run-abc "abcd"))) #\c)
(test (success-rem (run-abc "abcd")) "d")
(test (success-rem (run-abc "abc")) "")

;; échec à chaque position
(test (failure? (run-abc "xbcd")) #t) ;; premier
(test (failure? (run-abc "axcd")) #t) ;; deuxième
(test (failure? (run-abc "abxd")) #t) ;; troisième
(test (failure? (run-abc "ab")) #t) ;; entrée trop courte
(test (failure? (run-abc "")) #t)

;; chaque parseur reçoit le reste du précédent
(define (run-aaa input)
  ((Triple (ch #\a) (ch #\a) (ch #\a)) input))

(test (success? (run-aaa "aaab")) #t)
(test (success-rem (run-aaa "aaab")) "b")
(test (failure? (run-aaa "aab")) #t)
(test (failure? (run-aaa "aa")) #t)

;; avec des chiffres
(define (run-3d input)
  ((Triple pdigit pdigit pdigit) input))

(test (t1 (success-val (run-3d "123!"))) #\1)
(test (t2 (success-val (run-3d "123!"))) #\2)
(test (t3 (success-val (run-3d "123!"))) #\3)
(test (success-rem (run-3d "123!")) "!")
(test (failure? (run-3d "12a")) #t)
(test (failure? (run-3d "a12")) #t)

;; types différents : Number, Char, Char (c'est l'intérêt d'un vrai triplet)
;; (digit-value vient de utils.rkt)
(define (run-mixed input)
  ((Triple (Map digit-value pdigit) (ch #\,) pdigit) input))

(test (success? (run-mixed "7,8x")) #t)
(test (t1 (success-val (run-mixed "7,8x"))) (some 7)) ; un Number
(test (t2 (success-val (run-mixed "7,8x"))) #\,) ; un Char
(test (t3 (success-val (run-mixed "7,8x"))) #\8) ; un Char
(test (success-rem (run-mixed "7,8x")) "x")
(test (failure? (run-mixed "7;8")) #t)
(test (failure? (run-mixed "7,x")) #t)

;; l'ordre des éléments suit l'ordre des parseurs
(define (run-order input)
  ((Triple (ch #\x) pdigit (ch #\z)) input))

(test (t1 (success-val (run-order "x5z"))) #\x)
(test (t2 (success-val (run-order "x5z"))) #\5)
(test (t3 (success-val (run-order "x5z"))) #\z)

;; cohérence avec And imbriqué à droite : (a . (b . c))
;; Triple doit se comporter comme And a (And b c)
(define (run-nested input)
  ((And (ch #\a) (And (ch #\b) (ch #\c))) input))

(test (success? (run-abc "abcd")) (success? (run-nested "abcd")))
(test (failure? (run-abc "abxd")) (failure? (run-nested "abxd")))
(test (success-rem (run-abc "abcd")) (success-rem (run-nested "abcd")))
(test (t1 (success-val (run-abc "abcd"))) (fst (success-val (run-nested "abcd"))))
(test (t3 (success-val (run-abc "abcd"))) (snd (snd (success-val (run-nested "abcd")))))
(done "Triple")

;; =====================================================================
;; SEQUENCE : enchaîne une liste de parseurs, collecte les valeurs
;; =====================================================================

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

;; =====================================================================
;; MANY : zéro fois ou plus
;; =====================================================================

;; ---------- zéro correspondance : réussit quand même ----------
(test ((Many (ch #\a)) "bbb") (success empty "bbb")) ; rien consommé
(test ((Many (ch #\a)) "") (success empty "")) ; entrée vide
(test ((Many (ch #\a)) "b") (success empty "b"))

;; ---------- une ou plusieurs correspondances ----------
(test ((Many (ch #\a)) "abc") (success (list #\a) "bc"))
(test ((Many (ch #\a)) "aaab") (success (list #\a #\a #\a) "b"))
;; consomme toute l'entrée
(test ((Many (ch #\a)) "aaa") (success (list #\a #\a #\a) ""))
(test ((Many (ch #\a)) "a") (success (list #\a) ""))

;; ---------- s'arrête au premier échec ----------
;; le « a » après le « b » n'est pas lu
(test ((Many (ch #\a)) "aabaa") (success (list #\a #\a) "baa"))

;; ---------- ne rate jamais ----------
(test (success? ((Many (ch #\a)) "xyz")) #t)
(test (success? ((Many (ch #\a)) "")) #t)

;; ---------- l'ordre des valeurs est celui de la lecture ----------
(test ((Many (Any (list (ch #\a) (ch #\b)))) "abba!") (success (list #\a #\b #\b #\a) "!"))

;; ---------- avec Sequence : répète un motif de plusieurs caractères ----------
(test ((Many (Sequence (list (ch #\a) (ch #\b)))) "ababac")
      (success (list (list #\a #\b) (list #\a #\b)) "ac"))
;; un motif incomplet à la fin n'est pas consommé (retour à "ac")
(test ((Many (Sequence (list (ch #\a) (ch #\b)))) "abac") (success (list (list #\a #\b)) "ac"))

;; ---------- composition avec Map ----------
(test ((Map list->string (Many (ch #\a))) "aab") (success "aa" "b"))

;; ---------- avec return : ne pas écrire de test ----------
;; (Many (return 1)) boucle sans fin : return réussit sans rien consommer.
;; À éviter, ou à traiter plus tard.

(done "Many")

;; =====================================================================
;; MANY1 : une fois ou plus
;; =====================================================================
;; ---------- zéro correspondance : échoue (seule différence avec Many) ----------
(test (failure? ((Many1 (ch #\a)) "bbb")) #t)
(test (failure? ((Many1 (ch #\a)) "")) #t) ; entrée vide
(test (failure? ((Many1 (ch #\a)) "b")) #t)

;; ---------- une ou plusieurs correspondances ----------
(test ((Many1 (ch #\a)) "abc") (success (list #\a) "bc"))
(test ((Many1 (ch #\a)) "aaab") (success (list #\a #\a #\a) "b"))
;; consomme toute l'entrée
(test ((Many1 (ch #\a)) "a") (success (list #\a) ""))
(test ((Many1 (ch #\a)) "aaa") (success (list #\a #\a #\a) ""))

;; ---------- s'arrête au premier échec ----------
(test ((Many1 (ch #\a)) "aabaa") (success (list #\a #\a) "baa"))

;; ---------- le premier élément est obligatoire ----------
;; "baa" : le « a » après le « b » ne compte pas, c'est le début qui décide
(test (failure? ((Many1 (ch #\a)) "baa")) #t)

;; ---------- l'ordre des valeurs est celui de la lecture ----------
(test ((Many1 (Any (list (ch #\a) (ch #\b)))) "abba!") (success (list #\a #\b #\b #\a) "!"))

;; ---------- avec Sequence ----------
(test ((Many1 (Sequence (list (ch #\a) (ch #\b)))) "ababac")
      (success (list (list #\a #\b) (list #\a #\b)) "ac"))
;; un seul motif incomplet : échec dès le début
(test (failure? ((Many1 (Sequence (list (ch #\a) (ch #\b)))) "acb")) #t)

;; ---------- cohérence avec Many ----------
;; dès qu'il y a au moins une correspondance, Many1 et Many donnent pareil
(test ((Many1 (ch #\a)) "aab") ((Many (ch #\a)) "aab"))

;; ---------- composition avec Map ----------
(test ((Map list->string (Many1 (ch #\a))) "aab") (success "aa" "b"))
(test (failure? ((Map list->string (Many1 (ch #\a))) "bbb")) #t)

(done "Many1")

;; =====================================================================
;; Between
;; =====================================================================

(test
 ((Map list->string (Between (ch #\") (Many1 (Any (list (ch #\a) (ch #\b)))) (ch #\"))) "\"abbaabb\"")
 (success "abbaabb" ""))

;; ---------- cas simple : on ne garde que la valeur du milieu ----------
(test ((Between (ch #\() (ch #\a) (ch #\))) "(a)") (success #\a ""))

;; ---------- ce qui suit le fermant reste à lire ----------
(test ((Between (ch #\() (ch #\a) (ch #\))) "(a)bc") (success #\a "bc"))
;; un seul fermant est consommé
(test ((Between (ch #\() (ch #\a) (ch #\))) "(a))") (success #\a ")"))

;; ---------- délimiteurs manquants ou faux ----------
(test (failure? ((Between (ch #\() (ch #\a) (ch #\))) "a)")) #t) ; pas d'ouvrant
(test (failure? ((Between (ch #\() (ch #\a) (ch #\))) "(a")) #t) ; pas de fermant
(test (failure? ((Between (ch #\() (ch #\a) (ch #\))) "(a]")) #t) ; mauvais fermant
(test (failure? ((Between (ch #\() (ch #\a) (ch #\))) "[a)")) #t) ; mauvais ouvrant
(test (failure? ((Between (ch #\() (ch #\a) (ch #\))) "")) #t) ; entrée vide

;; ---------- le milieu est faux ----------
(test (failure? ((Between (ch #\() (ch #\a) (ch #\))) "(b)")) #t)
(test (failure? ((Between (ch #\() (ch #\a) (ch #\))) "()")) #t) ; milieu absent

;; ---------- milieu optionnel avec Many : vide accepté ----------
(test ((Between (ch #\() (Many (ch #\a)) (ch #\))) "()") (success empty ""))
(test ((Between (ch #\() (Many (ch #\a)) (ch #\))) "(aaa)") (success (list #\a #\a #\a) ""))

;; ---------- milieu obligatoire avec Many1 : vide refusé ----------
(test (failure? ((Between (ch #\() (Many1 (ch #\a)) (ch #\))) "()")) #t)
(test ((Between (ch #\() (Many1 (ch #\a)) (ch #\))) "(aa)") (success (list #\a #\a) ""))

;; ---------- imbriqués ----------
(test ((Between (ch #\() (Between (ch #\[) (ch #\a) (ch #\])) (ch #\))) "([a])") (success #\a ""))
;; mauvais ordre de fermeture
(test (failure? ((Between (ch #\() (Between (ch #\[) (ch #\a) (ch #\])) (ch #\))) "([a)]")) #t)

;; ---------- avec Sequence au milieu ----------
(test ((Between (ch #\() (Sequence (list (ch #\a) (ch #\b))) (ch #\))) "(ab)")
      (success (list #\a #\b) ""))
(test (failure? ((Between (ch #\() (Sequence (list (ch #\a) (ch #\b))) (ch #\))) "(a)")) #t)

(done "Between")

;; =====================================================================
;; SKIP : lance un parseur, jette sa valeur, renvoie (void)
;; =====================================================================

;; ---------- consomme comme le parseur d'origine ----------
(test ((Skip (ch #\a)) "abc") (success (void) "bc"))
(test ((Skip (ch #\a)) "a") (success (void) ""))
;; un seul caractère consommé
(test (success-rem ((Skip (ch #\a)) "aaa")) "aa")

;; ---------- la valeur est toujours (void) ----------
(test (success-val ((Skip (ch #\a)) "abc")) (void))
(test (success-val ((Skip (return 42)) "abc")) (void)) ; la valeur 42 est jetée

;; ---------- échoue exactement quand le parseur échoue ----------
(test (failure? ((Skip (ch #\a)) "bcd")) #t)
(test (failure? ((Skip (ch #\a)) "")) #t)

;; ---------- même reste que sans Skip ----------
(test (success-rem ((Skip (ch #\a)) "abc")) (success-rem ((ch #\a) "abc")))

;; ---------- avec return : ne consomme rien ----------
(test ((Skip (return 1)) "abc") (success (void) "abc"))

;; ---------- avec Many : zéro ou plusieurs espaces ----------
(test ((Skip (Many (ch #\space))) "   x") (success (void) "x"))
(test ((Skip (Many (ch #\space))) "x") (success (void) "x")) ; zéro espace : réussit
(test ((Skip (Many (ch #\space))) "") (success (void) ""))

;; ---------- dans une suite : la valeur ignorée n'apparaît pas ----------
(test ((And (Skip (Many (ch #\space))) (ch #\+)) "   +5") (success (pair (void) #\+) "5"))
;; ... mais le reste est bien celui d'après le « + »

;; ---------- Skip répété ----------
(test ((Skip (Skip (ch #\a))) "abc") (success (void) "bc"))

(done "Skip")

;; =====================================================================
;; MAP2 : enchaîne deux parseurs, applique f aux deux résultats
;; =====================================================================

;; ---------- succès ----------
(test ((Map2 (lambda (a b) (list a b)) (pchar #\a) (pchar #\b)) "abc") (success (list #\a #\b) "c"))

(test ((Map2 + pnum pnum) "34x") (success 7 "x"))

;; ---------- l'ordre des arguments de f : premier résultat d'abord ----------
(test ((Map2 - pnum pnum) "93") (success 6 ""))   ; 9 - 3, pas 3 - 9
(test ((Map2 (lambda (a b) a) pnum pnum) "12") (success 1 ""))   ; garde le premier
(test ((Map2 (lambda (a b) b) pnum pnum) "12") (success 2 ""))   ; garde le second

;; ---------- le type du résultat est celui de f ----------
(test ((Map2 (lambda (a b) (list->string (list a b))) (pchar #\a) (pchar #\b)) "abc")
      (success "ab" "c"))
(test ((Map2 pair pnum (pchar #\x)) "5x") (success (pair 5 #\x) ""))

(define (Map2-list a b) (list a b))

;; ---------- échecs : à chaque position ----------
(test (failure? ((Map2 Map2-list (pchar #\a) (pchar #\b)) "xb")) #t)   ; le premier échoue
(test (failure? ((Map2 Map2-list (pchar #\a) (pchar #\b)) "ax")) #t)   ; le second échoue
(test (failure? ((Map2 Map2-list (pchar #\a) (pchar #\b)) "a")) #t)    ; trop court
(test (failure? ((Map2 Map2-list (pchar #\a) (pchar #\b)) "")) #t)

;; ---------- backtracking : un échec ne consomme rien ----------
(test ((Or (Map2 Map2-list (pchar #\a) (pchar #\b))
           (Map2 Map2-list (pchar #\a) (pchar #\c)))
       "ac")
      (success (list #\a #\c) ""))

;; ---------- avec d'autres combinateurs ----------
(test ((Map2 cons pnum (Many pnum)) "123") (success (list 1 2 3) ""))
(test ((Map2 cons pnum (Many pnum)) "1") (success (list 1) ""))   ; Many vide
(test ((Map2 (lambda (xs c) c) (Many pnum) (pchar #\!)) "12!") (success #\! ""))
(test ((Map2 + (Map2 + pnum pnum) pnum) "123") (success 6 ""))   ; Map2 imbriqué
(test ((Many (Map2 + pnum pnum)) "1234") (success (list 3 7) ""))

(done "Map2")
;; =====================================================================
;; MAP3 : enchaîne trois parseurs, applique f aux trois résultats
;; =====================================================================

(define (Map3-list a b c) (list a b c))

;; ---------- succès ----------
(test ((Map3  Map3-list  (pchar #\a) (pchar #\b) (pchar #\c)) "abcd")
      (success (list #\a #\b #\c) "d"))
(test ((Map3 Map3-list (pchar #\a) (pchar #\b) (pchar #\c)) "abc")
      (success (list #\a #\b #\c) ""))
(test ((Map3 (lambda (a b c) (+ a c)) pnum (pchar #\+) pnum) "3+4") (success 7 ""))

;; ---------- l'ordre des arguments de f ----------
(test ((Map3 (lambda (a b c) a) pnum pnum pnum) "123") (success 1 ""))
(test ((Map3 (lambda (a b c) b) pnum pnum pnum) "123") (success 2 ""))
(test ((Map3 (lambda (a b c) c) pnum pnum pnum) "123") (success 3 ""))
(test ((Map3 (lambda (a b c) (- (- a b) c)) pnum pnum pnum) "931") (success 5 ""))   ; (9-3)-1

;; ---------- trois types différents ----------
(test ((Map3 (lambda (a b c) (pair a (pair b c))) pnum (pchar #\+) pnum) "1+2")
      (success (pair 1 (pair #\+ 2)) ""))

;; ---------- échecs : à chaque position ----------
(test (failure? ((Map3 Map3-list (pchar #\a) (pchar #\b) (pchar #\c)) "xbc")) #t)
(test (failure? ((Map3 Map3-list (pchar #\a) (pchar #\b) (pchar #\c)) "axc")) #t)
(test (failure? ((Map3 Map3-list (pchar #\a) (pchar #\b) (pchar #\c)) "abx")) #t)
(test (failure? ((Map3 Map3-list (pchar #\a) (pchar #\b) (pchar #\c)) "ab")) #t)
(test (failure? ((Map3 Map3-list (pchar #\a) (pchar #\b) (pchar #\c)) "a")) #t)
(test (failure? ((Map3 Map3-list (pchar #\a) (pchar #\b) (pchar #\c)) "")) #t)

;; ---------- backtracking ----------
(test ((Or (Map3 Map3-list (pchar #\a) (pchar #\b) (pchar #\c))
           (Map3 Map3-list (pchar #\a) (pchar #\b) (pchar #\d)))
       "abd")
      (success (list #\a #\b #\d) ""))

;; ---------- avec d'autres combinateurs ----------
(test ((Map3 (lambda (a b c) (+ a c)) pnum (Many pspace) pnum) "1   2") (success 3 ""))
(test ((Map3 (lambda (a b c) (+ a c)) pnum (Many pspace) pnum) "12") (success 3 ""))   ; zéro espace
(test ((Map3 (lambda (o x c) x) (pchar #\() pint (pchar #\))) "(-42)") (success -42 ""))
(test ((Many (Map3 (lambda (a b c) (+ a c)) pnum (pchar #\+) pnum)) "1+23+4")
      (success (list 3 7) ""))

(done "Map3")

;; =====================================================================
;; NOT : !e, réussit si p échoue, ne consomme jamais
;; =====================================================================

(define anyP (satisfy (lambda (c) #t)))   ; n'importe quel caractère

;; ---------- réussit quand p échoue, sans rien consommer ----------
(test ((Not (pchar #\a)) "bcd") (success (void) "bcd"))
(test ((Not (pchar #\a)) "x") (success (void) "x"))
(test ((Not (pchar #\a)) "") (success (void) ""))   ; p échoue sur le vide
(test ((Not (pstring "let")) "lex") (success (void) "lex"))
(test ((Not (pstring "let")) "le") (success (void) "le"))

;; ---------- échoue quand p réussit ----------
(test (failure? ((Not (pchar #\a)) "abc")) #t)
(test (failure? ((Not (pchar #\a)) "a")) #t)
(test (failure? ((Not (pstring "let")) "letter")) #t)
(test (failure? ((Not (pstring "<-")) "<- x")) #t)

;; ---------- il ne consomme jamais : le reste est l'entrée complète ----------
(test (success-rem ((Not (pchar #\a)) "bcd")) "bcd")
(test (success-rem ((Not (pchar #\a)) "")) "")

;; ---------- double négation : !!p équivaut à &p (teste sans consommer) ----------
(test ((Not (Not (pchar #\e))) "abc") (success (void) "abc"))
(test (failure? ((Not (Not (pchar #\a))) "xbc")) #t)

;; ---------- enchaîné avec un autre parseur ----------
;; "pas un a, puis un b"
(test ((Map2 (lambda (n c) c) (Not (pchar #\a)) (pchar #\b)) "bc")
      (success #\b "c"))
(test (failure? ((Map2 (lambda (n c) c) (Not (pchar #\a)) (pchar #\b)) "ab")) #t)
(test (failure? ((Map2 (lambda (n c) c) (Not (pchar #\a)) (pchar #\b)) "xb")) #t)   ; b absent

;; ---------- "n'importe quel caractère sauf" : !" puis anyP ----------
(test ((Map2 (lambda (n c) c) (Not (pchar #\")) anyP) "abc") (success #\a "bc"))
(test (failure? ((Map2 (lambda (n c) c) (Not (pchar #\")) anyP) "\"abc")) #t)

;; contenu d'une chaîne jusqu'au guillemet
(test ((Many (Map2 (lambda (n c) c) (Not (pchar #\")) anyP)) "abc\"def")
      (success (list #\a #\b #\c) "\"def"))
(test ((Many (Map2 (lambda (n c) c) (Not (pchar #\")) anyP)) "\"def")
      (success empty "\"def"))                                  ; vide
(test ((Many (Map2 (lambda (n c) c) (Not (pchar #\")) anyP)) "abc")
      (success (list #\a #\b #\c) ""))                          ; jusqu'à la fin

;; ---------- dans un choix : l'échec de ! ne consomme rien ----------
(test ((Or (Map2 (lambda (n c) c) (Not (pchar #\a)) (pchar #\b))
           (pchar #\a))
       "abc")
      (success #\a "bc"))

;; ---------- le cas des mots-clés : le mot, pas suivi d'un caractère d'identifiant ----------
(define (pkw s)
  (Map2 (lambda (w n) w) (pstring s) (Not (pchars-in id-chars))))

(test ((pkw "let") "let x") (success "let" " x"))
(test ((pkw "let") "let") (success "let" ""))            ; fin d'entrée
(test ((pkw "let") "let)") (success "let" ")"))
(test ((pkw "let") "let{") (success "let" "{"))
(test (failure? ((pkw "let") "letter")) #t)
(test (failure? ((pkw "let") "let1")) #t)                ; un chiffre prolonge l'identifiant
(test (failure? ((pkw "let") "let_")) #t)
(test ((pkw "true") "true false") (success "true" " false"))
(test (failure? ((pkw "true") "trueish")) #t)

;; caractères d'un identifiant dans une grammaire PEG (lettres, chiffres, _)
(peg-id-chars : String)
(define peg-id-chars
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")

;; ---------- l'identifiant qui n'est pas une tête de règle (grammaire PEG) ----------
(define pegIdP (pchars-in peg-id-chars))
(define primaryIdP (Map2 (lambda (id n) id) pegIdP (Not (pstring "<-"))))

(test (primaryIdP "foo bar") (success "foo" " bar"))
(test (primaryIdP "foo") (success "foo" ""))
(test (primaryIdP "foo)") (success "foo" ")"))
(test (failure? (primaryIdP "foo<-bar")) #t)   ; maintenant pour la bonne raison
(test (failure? (primaryIdP "A<-B")) #t)


(done "Not")