#lang plait

(require "lib/combinators.rkt"
         "lib/parsers.rkt")

(display ((Triple (Skip (Many (pchar #\space)))
                  (Many1 (Or (pchar #\a) (pchar #\b)))
                  (Skip (Many (pchar #\space))))
          "    ababab     "))
