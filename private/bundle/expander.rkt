#lang racket/base

;;; the Bundle language is a DSL for writing Rilouworld bundles

(provide
  (except-out (all-from-out racket/base)
              #%module-begin)
  (rename-out (module-begin #%module-begin))
  define-quest-actor
  (all-from-out rilouworld/private/quest/props))

(require
  (for-syntax racket/base
              syntax/parse)
  rilouworld/private/quest/props)

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(#%module-begin
        (provide (all-defined-out))
        expr ...)]))

(begin-for-syntax
  (define-syntax-class actor-attr
    (pattern (id:id contract:expr replace:keyword))
    (pattern (id:id contract:expr) #:with replace #'f))

  (define-syntax-class actor-event
    (pattern (id:id callback:expr))))

(define-syntax (define-quest-actor stx)
  (syntax-parse stx
    #:datum-literals (attributes)
    [(_ id (attributes attr:actor-attr ...) (events event:actor-event ...))
     #'(void)]))
