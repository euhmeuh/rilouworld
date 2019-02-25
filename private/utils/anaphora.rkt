#lang racket/base

(provide
  ;; create an anaphoric macro
  define-asyntax
  ;; create an anaphoric one rule macro
  define-asyntax-rule)

(require
  (for-syntax racket/base))

(define-syntax-rule (define-asyntax name (anaphora ...) (pattern template) ...)
  (define-syntax (name stx)
    (syntax-case stx ()
      [pattern
       (with-syntax [(anaphora (datum->syntax stx 'anaphora)) ...]
         (syntax template))] ...)))

(define-syntax-rule (define-asyntax-rule (name pattern ...) (anaphora ...) template)
  (define-asyntax name (anaphora ...)
    [(_ pattern ...) template]))
