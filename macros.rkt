#lang racket

(require mzlib/defmacro)
(defmacro let (decl body)
  (cons
   (cons 'lambda
         (cons (list (car decl)) body))
   (cdr decl)))

(let (x 42) (print x))