#lang racket

(define calculate (lambda () 42))

(require mzlib/defmacro)
;;(defmacro aif args
;;(list 'let* (list (list 'temp
;;(car args))
;;(list 'it 'temp))
;;(list 'if 'temp
;;(cadr args)
;;(caddr args))))
;;
;;(defmacro aif args
;;`(let*
;;((temp ,(car args))
;;(it
;;temp))
;;(if
;;temp
;;,(cadr args)
;;,(caddr args))))
;;
(define-syntax aif
  (syntax-rules ()
    ((aif cond then else)
     (let* ((temp cond)
            (it temp))
       (if temp 
           then 
           else)))))

(aif (calculate)
(print "it")
(error "does not compute"))