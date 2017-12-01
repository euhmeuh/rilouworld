#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out (module-begin #%module-begin)))

(define-syntax-rule (module-begin expr ...)
  (#%module-begin
    (provide dimension)
    (define dimension (quote expr ...))))
