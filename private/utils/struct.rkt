#lang racket/base

;;; utility functions and macros to work with structs

(provide
  ;; get struct fields as local variables
  with-values)

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse))

(define-for-syntax (make-getters stx instance struct-name fields)
  (datum->syntax stx
    (map (lambda (field)
           #`(#,field (#,(format-id stx "~a-~a" struct-name field) #,instance)))
         (syntax->list fields))))

(define-syntax (with-values stx)
  (syntax-parse stx
    #:datum-literals (from)
    [(_ <instance> (<field> ...) from <struct> <body> ...)
     #:with (getters ...) (make-getters stx #'<instance> #'<struct> #'(<field> ...))
     #'(let (getters ...)
         <body> ...)]))

(module+ test
  (require rackunit)

  (struct character (name size))
  (define my-char (character "Merdelin" 12))
  (check-equal?
    (with-values my-char (name size) from character
      (format "I am ~a and my size is ~a!" name size))
    "I am Merdelin and my size is 12!")

)
