#lang racket/base

;;; the Bundle language is a DSL for writing Rilouworld bundles

(provide
  (all-from-out racket/base)
  define-quest-actor
  actor-out
  (all-from-out rilouworld/private/quest/props))

(require
  (for-syntax racket/base
              racket/struct-info
              racket/syntax
              syntax/parse
              syntax/stx
              syntax/transformer
              rilouworld/private/quest/props-meta)
  racket/match
  rilouworld/quest
  rilouworld/private/quest/props
  rilouworld/private/core/event
  rilouworld/private/core/receiver)

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
             #:attr *** (quote-syntax ...)
             #:attr maybe-list (and (attribute list?) #'***)
             #:attr term-name (format-id #'<name> "*~a*" #'<name>)
             #:attr term-name-result (format-id #'term-name "~a.result" #'term-name)
             #:with term (if (attribute list?) #'(list term-name-result ***) #'term-name-result)

             #:attr verb (if (attribute <default>) #'~optional #'~once)
             #:attr term-class (if (attribute list?)
                                   #'any-exp
                                   (case (syntax-e #'<contract>)
                                     [(vec?) #'vec-exp]
                                     [(size?) #'size-exp]
                                     [(rect?) #'rect-exp]
                                     [else #'any-exp]))
             #:attr pattern #'(<name> (~var term-name term-class) (~? maybe-list))
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
  (struct metactor (parser struct-info attributes)
    #:property prop:procedure (struct-field-index parser)
    #:property prop:struct-info (lambda (self) (metactor-struct-info self))
    #:property prop:metattributes (lambda (self) (metactor-attributes self)))

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
;; - a macro also named <id> that has two behaviors:
;;   At compile-time, can be inherited to create larger macros
;;   At run-time, parses an actor into a struct instance
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
     #:with ((attr-name attr-pat attr-term) ...)
            (merge-attributes
              (if (attribute <parent>) #'<parent> #f)
              (if (attribute attributes?)
                #'((<own-attr>.<name> <own-attr>.parse-pattern <own-attr>.term) ...)
                #'()))

     ;; render all generic implementations
     #:with (methods ...) #'((#:methods <generic>.<id> [<generic>.<implem> ...]) ...)

     ;; specific implementation of gen:receiver for events
     #:attr receiver (and (attribute events?)
                          #'(#:methods gen:receiver
                             [(define (receiver-emit self ev)
                                (match-let ([(event type val) ev])
                                  (let ([val (if (list? val) val (list val))])
                                    (match type
                                      ['<event>.<type> (apply <event>.<callback> (cons self val))] ...
                                      [_ #t]))))]))

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

         (define-for-syntax (parse-id stx)
           (syntax-parse stx
             #:datum-literals (attr-name ...)
             [(_ (~alt attr-pat ...) ***)
              #'(internal-id attr-term ...)]))

         ;; we encapsulate the struct in our own identifier
         (define-syntax <id>
           (metactor
             parse-id
             (extract-struct-info (syntax-local-value #'internal-id))
             (~? (quote-syntax ((<own-attr>.<name>
                                 <own-attr>.parse-pattern
                                 <own-attr>.term) ...)) #'())))
         )]))

(define-syntax actor-out (make-rename-transformer #'struct-out))

;; --- Test area below ---

(module+ dolphin-test
  (provide (actor-out dolphin))
  (define-quest-actor dolphin
    (attributes
      (name string?)
      (children dolphin? #:list))))

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
    (fish (scales 'blue)
          (name "Java")
          (weight 5))
    (internal-fish "Java" 'blue 5))

  (check-equal?
    (fish (weight 12)
          (name "Big Red Fish")
          (scales 'red))
    (internal-fish "Big Red Fish" 'red 12))

  (check-equal?
    (fish-scales (fish (name "Bubble") (scales 'green) (weight 1)))
    'green)

  (define (submod-test)
    (local-require (submod ".." dolphin-test))

    (define skippy
      (dolphin
        (name "Skippy")
        (children
          (dolphin (name "Bumpy") (children))
          (dolphin (name "Groovy") (children)))))

    (check-equal? (dolphin-name skippy) "Skippy")
    (check-equal?
      (map (lambda (dolphin) (dolphin-name dolphin))
           (dolphin-children skippy))
      (list "Bumpy" "Groovy")))

  (submod-test)

)
