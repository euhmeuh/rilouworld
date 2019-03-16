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
              racket/syntax
              syntax/parse)
  racket/match
  rilouworld/private/core/receiver
  rilouworld/private/quest/props)

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(#%module-begin
        (provide (all-defined-out))
        expr ...)]))

(begin-for-syntax
  (define-literal-set props-literals (pos size rect))

  (define-syntax-class attr-exp
    (pattern (<name>:id
              <contract>:expr
              (~alt (~optional (~and list? #:list))
                    (~optional (~and mutable? #:mutable))
                    (~optional (~and private? #:private))
                    (~optional (~seq #:default <default>:expr))
                    ) ...)
             #:with term (format-id #'<name> "*~a*" #'<name>)
             #:with result (if (attribute mutable?)
                               #'(<name> #:mutable)
                               #'<name>)))

  (define-syntax-class event-exp
    (pattern (<type>:id <callback>:expr)))

  (define-splicing-syntax-class generic-exp
    (pattern (~seq <id>:id <implem>:expr ...)))

  (define (render-all-attributes lctx parent attributes)
    (displayln parent)
    (displayln attributes)
    (syntax/loc lctx ())))

;; This macro creates:
;; - a struct named <id> (with an optional parent)
;; - a macro named parse-<id> to parse it into a constructor usage
(define-syntax (define-quest-actor stx)
  (syntax-parse stx
    #:datum-literals (from attributes events implements)
    [(_ <id>
        (~optional (from <parent>))
        (~alt (~optional (~and attributes? (attributes <own-attr>:attr-exp ...)))
              (~optional (~and events? (events <event>:event-exp ...)))
              (implements <generic>:generic-exp)
              ) ...)

     ;; we find parent's attributes and merge them with our own
     #:with (attr:attr-exp ...) (render-all-attributes stx
                                  (if (attribute <parent>) #'<parent> #'actor)
                                  (if (attribute attributes?) #'(<own-attr> ...) #'()))

     ;; render all generic implementations
     #:with (methods ...) #'((#:methods <generic>.<id> [<generic>.<implem> ...]) ...)

     ;; specific implementation of gen:receiver for events
     #:with receiver (if (attribute events?)
                         #'(#:methods gen:receiver
                            [(define (receiver-emit self event)
                               (let-values ([(type val) (qualify-event event)])
                                 (match type
                                   ['<event>.<type> (<event>.<callback> self val)] ...
                                   [_ #t])))])
                         #'())

     ;; render a macro named parse-<id>
     #:with parse-id (format-id #'<id> "parse-~a" (syntax-e #'<id>))
     #:with *** (quote-syntax ...)

     #'(begin
         (struct <id> (~? <parent> actor) (~? (<own-attr>.result ...) ())
           (~@ . methods) ...
           (~@ . receiver))
         #;(define-syntax (parse-id stx)
           (syntax-parse stx
             #:literal-sets (props-literals)
             [(_ (~alt attr.result ...) ***)
              #'(<id> attr.term ...)]))
         )]))
