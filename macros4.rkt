#lang racket

;;(define-syntax (aif stx)
;;  (syntax-case stx ()
;;    ((aif test then else)
;;     (with-syntax ((it (datum->syntax #'aif 'it)))
;;       #'(let* ((temp test)
;;                (it temp))
;;           (if temp then else))))))

(require racket/stxparam)

(define-syntax-parameter it (syntax-rules ()))

(define-syntax aif
  (syntax-rules ()
    ((aif test then else)
     (let* ((temp test))
       (syntax-parameterize
        ((it (syntax-rules () ((_) temp))))
        (if temp then else))))))

(define calculate (lambda () 42))

(aif (calculate) (print (it)) (error "does not compute"))
 