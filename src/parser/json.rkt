#lang plait

(require "generators.rkt"
         "utils.rkt")

(json : Lang)
(define json
  (list
   (pair 'JSON (seq (list (ref 'WS) (ref 'Value) (not-p (one-of all-chars)))))
   (pair 'Value (alt (list (ref 'Object) (ref 'Array) (ref 'String) (ref 'Number) (ref 'Keyword))))
   (pair 'Object
         (seq (list (lit "{")
                    (ref 'WS)
                    (opt (seq (list (ref 'Member)
                                    (star (seq (list (lit ",") (ref 'WS) (ref 'Member)))))))
                    (lit "}")
                    (ref 'WS))))
   (pair 'Member (seq (list (ref 'String) (lit ":") (ref 'WS) (ref 'Value))))
   (pair 'Array
         (seq (list (lit "[")
                    (ref 'WS)
                    (opt (seq (list (ref 'Value)
                                    (star (seq (list (lit ",") (ref 'WS) (ref 'Value)))))))
                    (lit "]")
                    (ref 'WS))))
   (pair 'String (seq (list (lit "\"") (star (ref 'JChar)) (lit "\"") (ref 'WS))))
   (pair 'JChar
         (alt (list (seq (list (lit "\\") (one-of "\"\\/bfnrt")))
                    (seq (list (lit "\\u") (ref 'Hex) (ref 'Hex) (ref 'Hex) (ref 'Hex)))
                    (seq (list (not-p (one-of "\"\\")) (one-of all-chars))))))
   (pair 'Hex (one-of "0123456789abcdefABCDEF"))
   (pair 'Number
         (seq (list (opt (lit "-"))
                    (alt (list (lit "0") (seq (list (one-of "123456789") (star (one-of digits))))))
                    (opt (seq (list (lit ".") (plus (one-of digits)))))
                    (opt (seq (list (one-of "eE") (opt (one-of "-+")) (plus (one-of digits)))))
                    (ref 'WS))))
   (pair 'Keyword (seq (list (alt (list (lit "true") (lit "false") (lit "null"))) (ref 'WS))))
   (pair 'WS (star (one-of blank-chars)))))

(define json-parse (parser-str json))

(display (json-parse "{ \"data\" : 78 }"))

(display (json-parse "{ \"data\" : [7, \"hello world\", true, null] }"))