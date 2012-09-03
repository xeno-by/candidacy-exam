#lang racket

(require mzlib/defmacro)

(defmacro aif args
  (list 'let 
        (list (list 'it (car args)))
        (list 'if
              'it
              (cadr args)
              (caddr args))))

(define calculate (lambda () 42))
(aif (calculate)
  (print it)
  (error "does not compute"))