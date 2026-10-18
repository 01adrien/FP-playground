#lang plait

(require "core.rkt")

;; (And parser1 parser2)
;; On lance `parser1`, on garde `a`, on lance `parser2`,
;; on garde `b`, puis on renvoie la paire. Échoue dès que l'un des deux échoue.
(And : ((Parser 'a) (Parser 'b) -> (Parser ('a * 'b))))
(define (And parser1 parser2)
  (bind parser1 (lambda (a) (bind parser2 (lambda (b) (return (pair a b)))))))

;; (Or parser1 parser2)
;; essaie `parser1`, s'il échoue, essaie `parser2` sur la meme entrée de départ.
;; Les deux doivent produire le même type. Ne garde que le premier succès.
(Or : ((Parser 'a) (Parser 'a) -> (Parser 'a)))
(define (Or parser1 parser2)
  (lambda (str)
    (let ([r1 (parser1 str)])
      (if (failure? r1)
          (parser2 str)
          r1))))

;; (Map fn parser)
;; applique `fn` à la valeur produite par le `parser`.
;; Ne change pas ce qui est consommé, seulement la valeur renvoyée.
(Map : (('a -> 'b) (Parser 'a) -> (Parser 'b)))
(define (Map fn parser)
  (bind parser (lambda (x) (return (fn x)))))

;; (Any parsers)
;; essaie les `parsers` dans l'ordre, garde le premier qui réussit.
(Any : ((Listof (Parser 'a)) -> (Parser 'a)))
(define (Any parsers)
  (lambda (str)
    (cond
      [(empty? parsers) (failure "Any(): no suitable parser")]
      [else ((Or (first parsers) (Any (rest parsers))) str)])))

;; (Triple parser1 parser2 parser3)
;; enchaîne trois parseurs ; renvoie leurs valeurs en paire imbriquée (a . (b . c)).
(Triple : ((Parser 'a) (Parser 'b) (Parser 'c) -> (Parser ('a * ('b * 'c)))))
(define (Triple parser1 parser2 parser3)
  (bind parser1
        (lambda (a)
          (bind parser2 (lambda (b) (bind parser3 (lambda (c) (return (pair a (pair b c))))))))))

;; (Sequence parsers)
;; applique les `parsers` l'un après l'autre sur la même entrée
;; (chacun reprend là où le précédent s'est arrêté) et collecte leurs valeurs
;; dans une liste, dans l'ordre. Un seul échec fait échouer toute la séquence.
;; Pas d'accumulateur : la liste se construit au retour de la récursion
(Sequence : ((Listof (Parser 'a)) -> (Parser (Listof 'a))))
(define (Sequence parsers)
  (lambda (str)
    (cond
      [(empty? parsers) (success empty str)]
      [else
       ((bind (first parsers)
              (lambda (x) (Map (lambda (res) (cons x res)) (Sequence (rest parsers)))))
        str)])))

;; (many parser)
;; applique `parser` autant de fois que possible (zéro fois ou plus) et
;; renvoie la liste de toutes les valeurs, dans l'ordre de lecture.
;;
;; - many ne fait JAMAIS échouer : si `parser` ne convient pas dès le début,
;;   il réussit avec empty et rend la chaîne intacte.
;; - Il s'arrête au premier échec de `p`. Cet échec n'est pas une erreur :
;;   la chaîne restante est celle d'après le dernier succès.
;;   ex. (many (pchar #\a)) sur "aaab" -> succès (a a a), reste "b"
;;       (many (pchar #\a)) sur "bbb"  -> succès empty,   reste "bbb"
;;
;; Idée : « un `p` suivi de many p, sinon rien » (un Or entre les deux cas).
;; Contrairement à Sequence, la fin de la récursion ne dépend pas d'une
;; liste mais de l'entrée : c'est l'échec de `p` qui arrête la boucle.
;;
;; Attention : si `p` réussit sans rien consommer (ex. return), many ne
;; s'arrête jamais -> boucle infinie. À éviter ou à garder en tête.
;; (many : ((Parser 'a) -> (Parser (Listof 'a))))
