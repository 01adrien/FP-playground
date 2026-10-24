#lang plait

(require "generators.rkt" "utils.rkt")

(lisp : Lang)
(define lisp
  (list
   (pair 'Program
         (seq (list (ref 'WS)
                    (star (ref 'Expr))
                    (not-p (one-of all-chars)))))

   (pair 'Expr
         (alt (list (ref 'Quote)
                    (ref 'List)
                    (ref 'String)
                    (ref 'Number)
                    (ref 'Symbol))))

   (pair 'Quote
         (seq (list (lit "'") (ref 'WS) (ref 'Expr))))

   (pair 'List
         (seq (list (lit "(")
                    (ref 'WS)
                    (star (ref 'Expr))
                    (lit ")")
                    (ref 'WS))))

   (pair 'String
         (seq (list (lit "\"")
                    (star (seq (list (not-p (one-of "\"")) (one-of all-chars))))
                    (lit "\"")
                    (ref 'WS))))

   (pair 'Number
         (seq (list (opt (lit "-"))
                    (plus (one-of digits))
                    (opt (seq (list (lit ".") (plus (one-of digits)))))
                    (not-p (ref 'SymChar))
                    (ref 'WS))))

   (pair 'Symbol
         (seq (list (plus (ref 'SymChar)) (ref 'WS))))

   (pair 'SymChar
         (seq (list (not-p (one-of " \t\r\n()\";'")) (one-of all-chars))))

   (pair 'WS
         (star (alt (list (one-of blank-chars) (ref 'Comment)))))

   (pair 'Comment
         (seq (list (lit ";")
                    (star (seq (list (not-p (one-of "\n")) (one-of all-chars)))))))))



