#lang racket

(require mzlib/defmacro)

;;(defmacro let (decl body)
;;  (cons
;;   (cons 'lambda
;;         (cons (list (car decl)) body))
;;   (cdr decl)))

;;(defmacro let (decl body)
;;  `((lambda (,(car decl)) ,body) ,(cadr decl)))

;;(defmacro let (decls body)
;;  (cons
;;   (cons 'lambda
;;         (cons (map car decls)
;;               body))
;;   (map cadr decls)))

;;(defmacro let args
;;  (cons
;;   (cons 'lambda
;;         (cons (map car (car args))
;;               (cdr args)))
;;   (map cadr (car args))))

;;(defmacro let (decls body)
;;  `((lambda ,(map car decls) ,body) ,@(map cadr decls)))

(define-syntax let
  (syntax-rules ()
    ((let ((i e) ...) b ...)
     ((lambda (i ...) b ...) e ...))))

(let ((x 40) (y 2)) (print (+ x y)))