#lang plait

;; ASCII imprimable (de l'espace au tilde)
(define ascii-printable
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

;; blancs de contrôle
(define control-blanks "\t\n\r")

;; lettres accentuées et signes français courants
(define french-chars "àâäçéèêëîïôöùûüÿœæÀÂÄÇÉÈÊËÎÏÔÖÙÛÜŸŒÆ«»€’")

;; tous les caractères reconnus par "."
(define all-chars
  (string-append ascii-printable (string-append control-blanks french-chars)))

(define digits "0123456789")
(define blank-chars " \t\r\n")