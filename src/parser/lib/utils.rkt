#lang plait

;; Vrai si la chaîne est vide "".
(empty-string? : (String -> Boolean))
(define (empty-string? str)
  (string=? str ""))

;; Concatène une liste de chaînes en une seule : '("a" "bc" "d") -> "abcd".
(string-concat : ((Listof String) -> String))
(define (string-concat strs)
  (cond
    [(empty? strs) ""]
    [else (string-append (first strs) (string-concat (rest strs)))]))

;; Convertit un caractère en chaîne d'un caractère : #\a -> "a".
(char->string : (Char -> String))
(define (char->string c)
  (list->string (list c)))

;; La chaîne sans son premier caractère : "abc" -> "bc".
(string-rest : (String -> String))
(define (string-rest s)
  (if (empty-string? s)
      ""
      (substring s 1 (string-length s))))

;; Le premier caractère d'une chaîne : "abc" -> #\a.
(string-first : (String -> (Optionof Char)))
(define (string-first s)
  (if (empty-string? s)
      (none)
      (some (first (string->list s)))))

;; Table chiffre -> valeur.
(define digit-table
  (hash (list (pair #\0 0)
              (pair #\1 1)
              (pair #\2 2)
              (pair #\3 3)
              (pair #\4 4)
              (pair #\5 5)
              (pair #\6 6)
              (pair #\7 7)
              (pair #\8 8)
              (pair #\9 9))))

;; #\7 -> (some 7), #\a -> (none)
(digit-value : (Char -> (Optionof Number)))
(define (digit-value c)
  (hash-ref digit-table c))
