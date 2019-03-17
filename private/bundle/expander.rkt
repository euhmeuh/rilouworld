#lang racket/base

;;; the Bundle language is a DSL for writing Rilouworld bundles

(provide
  (except-out (all-from-out racket/base)
              #%module-begin)
  (rename-out (module-begin #%module-begin))
  define-quest-actor

  (all-from-out rilouworld/private/quest/props)
  (all-from-out rilouworld/private/core/receiver)
  (all-from-out rilouworld/private/core/sprite))

(require
  (for-syntax racket/base
              racket/match
              racket/struct-info
              racket/syntax
              syntax/parse
              syntax/stx
              syntax/transformer
              rilouworld/private/quest/props-meta)
  racket/match
  rilouworld/quest
  rilouworld/private/quest/props
  rilouworld/private/core/receiver
  rilouworld/private/core/sprite)

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ expr ...)
     #'(#%module-begin
        expr ...)]))

(begin-for-syntax
  (define-syntax-class attr-exp
    (pattern (<name>:id
              <contract>:expr
              (~alt (~optional (~and list? #:list))
                    (~optional (~and mutable? #:mutable))
                    (~optional (~and private? #:private))
                    (~optional (~seq #:default <default>:expr))
                    ) ...)
             ;; used to generate the macro parse-<id>
             #:with *** (quote-syntax ...)
             #:attr maybe-list (and (attribute list?) #'***)
             #:with term-name (format-id #'<name> "*~a*" #'<name>)
             #:with term (if (attribute list?) #'(list term-name ***) #'term-name)
             #:with verb (if (attribute <default>) #'~optional #'~once)
             #:with pattern (match (syntax-e #'<name>)
                              ;; handle special cases pos, size and rect
                              ['pos #'(~var term-name pos-exp)]
                              ['size #'(~var term-name size-exp)]
                              ['rect #'(~var term-name rect-exp)]
                              [_ #'(<name> term-name (~? maybe-list))])
             #:with parse-pattern #'(verb pattern)

             ;; used to generate the struct internal-<id>
             #:with struct-pattern (if (attribute mutable?)
                                     #'(<name> #:mutable)
                                     #'<name>)))

  (define-syntax-class event-exp
    (pattern (<type>:id <callback>:expr)))

  (define-splicing-syntax-class generic-exp
    (pattern (~seq <id>:id <implem>:expr ...)))

  (define-values (prop:metattributes metattributes? metattributes-ref)
                 (make-struct-type-property 'metattributes))

  ;; wrapper around the struct definition
  (struct metactor (normal struct-info attributes)
    #:property prop:procedure (struct-field-index normal)
    #:property prop:struct-info (lambda (self) (metactor-struct-info self))
    #:property prop:metattributes (lambda (self) (metactor-attributes self)))

  (define (make-var-like-transformer id)
    (set!-transformer-procedure (make-variable-like-transformer id)))

  (define (extract-metattributes stx)
    (and stx (let ([meta (syntax-local-value stx)])
               ((metattributes-ref meta) meta))))

  (define (stx-append stx-list-a stx-list-b)
    (datum->syntax stx-list-a
      (append (stx->list stx-list-a)
              (stx->list stx-list-b))))

  (define (merge-attributes parent attributes)
    (define parent-attrs (or (extract-metattributes parent) #'()))
    (stx-append parent-attrs attributes)))

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
     #:with ((attr term name) ...)
            (merge-attributes
              (if (attribute <parent>) #'<parent> #f)
              (if (attribute attributes?)
                #'((<own-attr>.parse-pattern <own-attr>.term <own-attr>.<name>) ...)
                #'()))

     ;; render all generic implementations
     #:with (methods ...) #'((#:methods <generic>.<id> [<generic>.<implem> ...]) ...)

     ;; specific implementation of gen:receiver for events
     #:attr receiver (and (attribute events?)
                          #'(#:methods gen:receiver
                             [(define (receiver-emit self event)
                                (let-values ([(type val) (qualify-event event)])
                                  (match type
                                    ['<event>.<type> (<event>.<callback> self val)] ...
                                    [_ #t])))]))

     ;; render a macro named parse-<id>
     #:with parse-id (format-id #'<id> "parse-~a" (syntax-e #'<id>))
     #:with internal-id (format-id #'<id> "internal-~a" (syntax-e #'<id>))
     #:with *** (quote-syntax ...)

     #'(begin
         (struct <id> (~? <parent> actor) (~? (<own-attr>.struct-pattern ...) ())
           #:name internal-id #:constructor-name internal-id
           #:transparent
           (~@ . methods) ...
           (~? (~@ . receiver)))

         ;; we encapsulate the struct in our own identifier
         (define-syntax <id>
           (metactor
             (make-var-like-transformer #'internal-id)
             (extract-struct-info (syntax-local-value #'internal-id))
             (~? (quote-syntax ((<own-attr>.parse-pattern
                                 <own-attr>.term
                                 <own-attr>.<name>) ...)) #'())))

         (define-syntax (parse-id stx)
           (syntax-parse stx
             #:datum-literals (name ...)
             [(_ (~alt attr ...) ***)
              #'(<id> term ...)]))
         )]))

(module+ test
  (require rackunit)

  (define-quest-actor animal
    (attributes
      (name string?)))

  (define-quest-actor fish (from animal)
    (attributes
      (scales symbol?)
      (weight number?)))

  (check-equal?
    (parse-fish (scales 'blue)
                (name "Java")
                (weight 5))
    (fish "Java" 'blue 5))

  (check-equal?
    (parse-fish (weight 12)
                (name "Big Red Fish")
                (scales 'red))
    (fish "Big Red Fish" 'red 12))

)
