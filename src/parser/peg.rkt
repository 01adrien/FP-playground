#lang plait

(require "generators.rkt"
         "utils.rkt")

(define letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define digits "0123456789")
(define octal "01234567")

;; (any-char) -> .   (suppose all-chars défini comme chez toi)
(any-char : (-> Grammar))
(define (any-char)
  (one-of all-chars))

;; (tok s) -> 's' Spacing   (un terminal qui avale les blancs derrière lui)
(tok : (String -> Grammar))
(define (tok s)
  (seq (list (lit s) (ref 'Spacing))))

(peg : Lang)
(define peg
  (list
   ;; ---------- Hierarchical syntax ----------
   (pair 'Grammar (seq (list (ref 'Spacing) (plus (ref 'Definition)) (ref 'EndOfFile))))
   (pair 'Definition (seq (list (ref 'Identifier) (ref 'LEFTARROW) (ref 'Expression))))
   (pair 'Expression (seq (list (ref 'Sequence) (star (seq (list (ref 'SLASH) (ref 'Sequence)))))))
   (pair 'Sequence (star (ref 'Prefix)))
   (pair 'Prefix (seq (list (opt (alt (list (ref 'AND) (ref 'NOT)))) (ref 'Suffix))))
   (pair 'Suffix
         (seq (list (ref 'Primary) (opt (alt (list (ref 'QUESTION) (ref 'STAR) (ref 'PLUS)))))))
   (pair 'Primary
         (alt (list (seq (list (ref 'Identifier) (not-p (ref 'LEFTARROW))))
                    (seq (list (ref 'OPEN) (ref 'Expression) (ref 'CLOSE)))
                    (ref 'Literal)
                    (ref 'Class)
                    (ref 'DOT))))
   ;; ---------- Lexical syntax ----------
   (pair 'Identifier (seq (list (ref 'IdentStart) (star (ref 'IdentCont)) (ref 'Spacing))))
   (pair 'IdentStart (one-of (string-append letters "_")))
   (pair 'IdentCont (alt (list (ref 'IdentStart) (one-of digits))))
   (pair 'Literal
         (alt (list (seq (list (lit "'")
                               (star (seq (list (not-p (lit "'")) (ref 'Char))))
                               (lit "'")
                               (ref 'Spacing)))
                    (seq (list (lit "\"")
                               (star (seq (list (not-p (lit "\"")) (ref 'Char))))
                               (lit "\"")
                               (ref 'Spacing))))))
   (pair
    'Class
    (seq
     (list (lit "[") (star (seq (list (not-p (lit "]")) (ref 'Range)))) (lit "]") (ref 'Spacing))))
   (pair 'Range (alt (list (seq (list (ref 'Char) (lit "-") (ref 'Char))) (ref 'Char))))
   (pair 'Char
         (alt (list (seq (list (lit "\\") (one-of "nrt'\"[]\\")))
                    (seq (list (lit "\\") (one-of "012") (one-of octal) (one-of octal)))
                    (seq (list (lit "\\") (one-of octal) (opt (one-of octal))))
                    (seq (list (not-p (lit "\\")) (any-char))))))
   ;; ---------- Terminaux ----------
   (pair 'LEFTARROW (tok "<-"))
   (pair 'SLASH (tok "/"))
   (pair 'AND (tok "&"))
   (pair 'NOT (tok "!"))
   (pair 'QUESTION (tok "?"))
   (pair 'STAR (tok "*"))
   (pair 'PLUS (tok "+"))
   (pair 'OPEN (tok "("))
   (pair 'CLOSE (tok ")"))
   (pair 'DOT (tok "."))
   ;; ---------- Blancs ----------
   (pair 'Spacing (star (alt (list (ref 'Space) (ref 'Comment)))))
   (pair
    'Comment
    (seq (list (lit "#") (star (seq (list (not-p (ref 'EndOfLine)) (any-char)))) (ref 'EndOfLine))))
   (pair 'Space (alt (list (lit " ") (lit "\t") (ref 'EndOfLine))))
   (pair 'EndOfLine (alt (list (lit "\r\n") (lit "\n") (lit "\r"))))
   (pair 'EndOfFile (not-p (any-char)))))

(define peg-parser (parser peg))

(peg-parser
"Config <- Line* !.
Line    <- Blank / Comment / Section / Entry
Blank   <- HS EOL
Comment <- HS [#;] (!EOL .)* End
Section <- HS '[' HS Name HS ']' HS End
Entry   <- HS Key HS '=' HS Value HS End
Name    <- [a-zA-Z_] [-a-zA-Z0-9_.]*
Key     <- [a-zA-Z_] [-a-zA-Z0-9_.]*
Value   <- Quoted / Bare
Quoted  <- '\"' ('\\\\' . / ![\"\\n] .)* '\"'
Bare    <- (![#;\\r\\n] .)*
HS      <- [ \\t]*
EOL     <- '\\r\\n' / '\\n' / '\\r'
End     <- EOL / !.
") 
