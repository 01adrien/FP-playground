#lang plait

(require "lib/combinators.rkt"
         "lib/parsers.rkt")

(define p (Sequence (list (pchar #\A) (pchar #\B) (pchar #\C))))
(display (p "ABC"))
