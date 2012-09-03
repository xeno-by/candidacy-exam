#lang racket

(require mzlib/defmacro)

(defmacro let args
  (cons
   (cons 'lambda
         (cons (list (caar args))
               (cdr args)))
   (cdar args)))

(let (x 42) (print x))