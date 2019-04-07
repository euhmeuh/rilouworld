#lang racket/base

;;; the Quest language is a DSL for writing Rilouworlds

(provide #%app #%datum #%top quote lambda
         (rename-out [module-begin #%module-begin])
         vec
         size
         rect
         (rename-out [parse-world world])
         (rename-out [parse-zone zone])
         (rename-out [load-quest ->]))

(require
  (for-syntax
    racket/base
    racket/syntax
    syntax/parse
    rilouworld/private/quest/props-meta)
  (only-in rilouworld/quest
    vec
    size
    rect
    world
    zone
    load-quest))

(define-for-syntax (bundle-path name)
  (format-id name "rilouworld/bundles/~a" name))

(define-syntax (module-begin stx)
  (syntax-parse stx
    #:datum-literals (from use)
    [(_ (from <bundle> use <actor> ...) ... <expr>)
     #:with (path ...) (datum->syntax stx
                         (map bundle-path
                              (syntax->list #'(<bundle> ...))))
     #'(#%module-begin
         (provide object)
         (require (only-in path <actor> ...)) ...
         (define object <expr>))]))

(define-syntax (parse-world stx)
  (syntax-parse stx
    #:datum-literals (name uuid version
                      changelog authors
                      description resources zones)
    [(_ (name <name>:str)
        (~alt (~optional (uuid <uuid>:str)
                         #:defaults ([<uuid> #'#f]))
              (~optional (version <version>:str)
                         #:defaults ([<version> #'#f]))
              (~optional (description <desc>:str)
                         #:defaults ([<desc> #'#f]))
              (~optional (changelog <change>:change-exp ...))
              (~optional (authors <author>:author-exp ...))
              (~optional (resources <res>:resource-exp ...))
              (~optional (zones <zone>:expr ...))) ...)
     #'(let ([<res>.<id> <res>.result] ...)
         (world <name>
                <uuid>
                <version>
                <desc>
                (~? (list <change>.result ...) '())
                (~? (list <author>.result ...) '())
                (~? (list <res>.<id> ...) '())
                (~? (list <zone> ...) '())
                #f ; current-zone
                ))]))

(define-syntax (parse-zone stx)
  (syntax-parse stx
    #:datum-literals (name map type outside inside rectangles rect actors)
    [(_ <id>:id
        (~alt (~once (name <name>:str))
              (~optional (map <map>:str)
                         #:defaults ([<map> #'#f]))
              (~optional (type (~and <type> (~or outside inside)))
                         #:defaults ([<type> #'outside]))
              (~once (rectangles (rect <rectangle>:rect-exp) ...))
              (~once (actors <actor>:expr ...))) ...)
     #'(zone '<id>
             <name>
             <map>
             '<type>
             (list <rectangle>.result ...)
             (list <actor> ...))]))
