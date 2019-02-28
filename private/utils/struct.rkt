#lang racket/base

;;; utility functions and macros to work with structs

(provide
  ;; get struct fields as local variables
  with-values)

(require
  (for-syntax racket/base))

(define-for-syntax (make-id stx template . ids)
  (define str (apply format template (map syntax->datum ids)))
  (datum->syntax stx (string->symbol str)))

(define-for-syntax (make-getters stx object struct-name values)
  (for/list ([value (syntax->list values)])
    `(,value (,(make-id stx "~a-~a" struct-name value) ,object))))

;; Example:
;;
;; (struct ingredient (name taste price color))
;; (define carrot (ingredient "Carrot" 'good 2.0 'orange))
;;
;; (with-values carrot (name taste) from ingredient
;;   (displayln (format "This is a ~a that tastes ~a!" name taste))
;;
(define-syntax (with-values stx)
  (syntax-case stx (from)
    [(_ s (value ...) from struct body ...)
     (with-syntax ([getters (make-getters stx #'s #'struct #'(value ...))])
       #'(let getters
           body ...))]))

#| test
(struct character (name size))
(define s (character "Merdelin" 12))
(with-values s (name size) from character
  (displayln (format "I am ~a and my size is ~a!" name size)))
|#