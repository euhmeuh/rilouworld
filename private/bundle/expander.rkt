#lang racket/base

;;; the Bundle language is a DSL for writing Rilouworld bundles

(provide
  (all-from-out racket/base)
  (all-from-out rilouworld/quest)
  define-quest-actor
  actor-out)

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
  rilouworld/private/core/event
  rilouworld/private/core/receiver)

(begin-for-syntax
  (define (get-field-class contract)
    (case (syntax-e contract)
      [(vec?) #'vec-exp]
      [(size?) #'size-exp]
      [(rect?) #'rect-exp]
      [else #'any-exp]))

  (define-syntax-class attr-exp
    (pattern (<name>:id
              <contract>:expr
              (~alt (~optional (~and list? #:list))
                    (~optional (~and mutable? #:mutable))
                    (~optional (~or (~seq #:private <private>:expr)
                                    (~seq #:optional <optional>:expr)))
                    ) ...)
      #:attr default (or (and (attribute <private>) #'<private>)
                         (and (attribute <optional>) #'<optional>))
      #:attr *? (quote-syntax ~?)
      #:attr *** (quote-syntax ...)
      #:attr maybe-list (and (attribute list?) #'***)

      #:attr field-name (format-id #'<name> "*~a*" #'<name>)
      #:attr field-result (format-id #'field-name "~a.result" #'field-name)
      #:attr field-class (if (attribute list?)
                             #'any-exp
                             (get-field-class #'<contract>))
      #:attr amount (if (attribute <optional>) #'~optional #'~once)

      #:with pattern (if (attribute <private>)
                         #'(~not _) ;; match nothing
                         #'(amount ((~datum <name>)
                                    (~var field-name field-class)
                                    (~? maybe-list))))
      #:with template (if (attribute <private>)
                          #'default
                          #`(*? #,(if (attribute list?)
                                      #'(list field-result ***)
                                      #'field-result)
                                (~? default)))

      #:with struct-field (if (attribute mutable?)
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
               ((metattributes-ref meta) meta)))))

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
        (~alt (~optional (~and attributes? (attributes <attr>:attr-exp ...)))
              (~optional (~and events? (events <event>:event-exp ...)))
              (implements <generic>:generic-exp)
              ) ...)

     #:with ((parent-attr-pattern
              parent-attr-template) ...)
            (if (attribute <parent>)
                (extract-metattributes #'<parent>)
                #'())

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
         (struct <id> (~? <parent> actor) (~? (<attr>.struct-field ...) ())
           #:name internal-id
           #:constructor-name internal-id
           #:transparent
           (~@ . methods) ...
           (~? (~@ . receiver)))

         (define-for-syntax (parse-id stx)
           (syntax-parse stx
             [(_ (~alt parent-attr-pattern ...
                       (~? (~@ . (<attr>.pattern ...)))
                       ) ***)
              #'(internal-id parent-attr-template ...
                             (~? (~@ . (<attr>.template ...))))]))

         ;; we encapsulate the struct in our own identifier
         (define-syntax <id>
           (metactor
             parse-id
             (extract-struct-info (syntax-local-value #'internal-id))
             (quote-syntax
               ((parent-attr-pattern parent-attr-template) ...
                (~? (~@ . ((<attr>.pattern <attr>.template) ...)))))))
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
      (weight number? #:optional 5)))

  (check-equal?
    (fish (scales 'blue)
          (name "Java"))
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
