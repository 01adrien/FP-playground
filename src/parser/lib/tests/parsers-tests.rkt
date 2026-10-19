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
(test (pspace " abc") (success #\space "abc"))
(test (pspace " ") (success #\space ""))

;; ---------- un seul espace est consommé ----------
(test (pspace "  x") (success #\space " x"))
(test (pspace "   ") (success #\space "  "))

;; ---------- rejets ----------
(test (failure? (pspace "abc")) #t)
(test (failure? (pspace "")) #t)
(test (failure? (pspace "1 ")) #t) ; l'espace n'est pas en tête
;; les autres blancs ne sont PAS des #\space
(test (failure? (pspace "\t")) #t)
(test (failure? (pspace "\n")) #t)

;; ---------- avec Many : le cas qui t'intéresse vraiment ----------
(test ((Many pspace) "   x") (success (list #\space #\space #\space) "x"))
(test ((Many pspace) "x") (success empty "x")) ; zéro espace : succès, pas échec
(test ((Many pspace) "") (success empty ""))

(done "spacep")

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

(done "pnum")
;; =====================================================================
;; PINT : entier positif ou négatif (un "-" optionnel, puis 1+ chiffres)
;; =====================================================================

;; ---------- positifs ----------
(test (pint "7") (success 7 ""))
(test (pint "42") (success 42 ""))
(test (pint "1234567890") (success 1234567890 ""))
(test (pint "42abc") (success 42 "abc"))
(test (pint "12 34") (success 12 " 34")) ; s'arrête à l'espace

;; ---------- négatifs ----------
(test (pint "-7") (success -7 ""))
(test (pint "-42") (success -42 ""))
(test (pint "-42abc") (success -42 "abc"))
(test (pint "-12 34") (success -12 " 34"))

;; ---------- zéros ----------
(test (pint "0") (success 0 ""))
(test (pint "-0") (success 0 "")) ; -0 vaut 0
(test (pint "007") (success 7 "")) ; zéros en tête acceptés
(test (pint "-007") (success -7 ""))
(test (pint "100") (success 100 "")) ; le 0 final compte (piège sur la valeur)

;; ---------- le reste après l'entier est intact ----------
(test (pint "5+3") (success 5 "+3"))
(test (pint "5-3") (success 5 "-3")) ; le "-" après est un reste, pas un signe
(test (pint "1.5") (success 1 ".5")) ; pas de décimaux

;; ---------- rejets ----------
(test (failure? (pint "")) #t)
(test (failure? (pint "abc")) #t)
(test (failure? (pint "-")) #t) ; signe sans chiffre
(test (failure? (pint "-abc")) #t)
(test (failure? (pint "--5")) #t) ; double signe
(test (failure? (pint "- 5")) #t) ; espace entre le signe et le chiffre
(test (failure? (pint " 5")) #t) ; espace devant (le token viendra après)

(done "pint")

;; =====================================================================
;; PID : identifiant (une suite de caractères pris dans id-chars)
;; =====================================================================

;; ---------- succès ----------
(test (pid "x") (success "x" ""))
(test (pid "foo") (success "foo" ""))
(test (pid "FooBar") (success "FooBar" "")) ; majuscules
(test (pid "x1") (success "x1" "")) ; chiffre après une lettre
(test (pid "snake_case") (success "snake_case" ""))
(test (pid "long_identifier_with_many_chars_123") (success "long_identifier_with_many_chars_123" ""))

;; ---------- il s'arrête au premier caractère interdit ----------
(test (pid "foo bar") (success "foo" " bar")) ; espace
(test (pid "foo}") (success "foo" "}")) ; accolade
(test (pid "foo)") (success "foo" ")"))
(test (pid "foo(bar)") (success "foo" "(bar)"))
(test (pid "foo\"bar") (success "foo" "\"bar")) ; guillemet
(test (pid "foo;bar") (success "foo" ";bar"))

;; ---------- rejets ----------
(test (failure? (pid "")) #t)
(test (failure? (pid " foo")) #t) ; espace devant
(test (failure? (pid "{foo")) #t)
(test (failure? (pid "\"foo\"")) #t)

;; ---------- un identifiant peut commencer par un chiffre (pour l'instant) ----------
;; pchars-in ne distingue pas le premier caractère : "123" est un pid valide.
;; Quand tu voudras interdire ça, ce test devra devenir (failure? ...).
(test (pid "123") (success "123" ""))
(test (pid "1abc") (success "1abc" ""))

;; ---------- conflit avec l'arithmétique (si + et - sont dans id-chars) ----------
;; à supprimer ou inverser selon le contenu de ton id-chars :
(test (pid "x+1") (success "x+1" "")) ; un seul identifiant
(test (pid "a-b") (success "a-b" ""))

;; ---------- avec Many ----------
(test ((Many (Between (Many pspace) pid (Many pspace))) "a b  c ") (success (list "a" "b" "c") ""))

(done "pid")

;; =====================================================================
;; PSTRING-LITERAL : chaîne entre guillemets, renvoie le contenu
;; =====================================================================

;; ---------- succès ----------
(test (pstring-literal "\"abc\"") (success "abc" ""))
(test (pstring-literal "\"a\"") (success "a" ""))
(test (pstring-literal "\"hello world\"") (success "hello world" "")) ; espaces autorisés
(test (pstring-literal "\"  x  \"") (success "  x  " "")) ; espaces conservés
(test (pstring-literal "\"abc\" reste") (success "abc" " reste"))
(test (pstring-literal "\"abc\"def") (success "abc" "def"))

;; ---------- les guillemets ne font pas partie du résultat ----------
(test (string-length (success-val (pstring-literal "\"abc\""))) 3)

;; ---------- caractères autorisés dans la chaîne ----------
(test (pstring-literal "\"{+ 1 2}\"") (success "{+ 1 2}" "")) ; accolades
(test (pstring-literal "\"a,b;c:d\"") (success "a,b;c:d" ""))
(test (pstring-literal "\"123\"") (success "123" ""))
(test (pstring-literal "\"it's\"") (success "it's" "")) ; apostrophe

;; ---------- une seule chaîne est lue ----------
(test (pstring-literal "\"ab\"\"cd\"") (success "ab" "\"cd\""))

;; ---------- rejets ----------
(test (failure? (pstring-literal "")) #t)
(test (failure? (pstring-literal "abc")) #t) ; pas de guillemets
(test (failure? (pstring-literal "\"abc")) #t) ; pas de guillemet fermant
(test (failure? (pstring-literal "abc\"")) #t) ; pas de guillemet ouvrant
(test (failure? (pstring-literal "\"")) #t) ; un seul guillemet
(test (failure? (pstring-literal " \"abc\"")) #t) ; espace devant

;; ---------- chaîne vide : échoue avec Many1 ----------
;; si tu passes à Many (zéro ou plus), ce test devient (success "" "")
(test (failure? (pstring-literal "\"\"")) #t)

;; ---------- caractères interdits dans string-chars ----------
(test (failure? (pstring-literal "\"a\\b\"")) #t) ; backslash
(test (failure? (pstring-literal "\"a\nb\"")) #t) ; retour à la ligne
;; guillemet échappé : pas géré, la chaîne s'arrête à \
(test (failure? (pstring-literal "\"a\\\"b\"")) #t)

(done "pstring-literal")
