#lang racket/base

;;; the Quest language is a DSL for writing Rilouworlds

(provide #%app #%datum #%top quote
         (rename-out [module-begin #%module-begin])
         pos
         size
         rect
         (rename-out [parse-world world])
         (rename-out [parse-zone zone])
         (rename-out [load-quest ->]))

(require
  (for-syntax
    racket/base
    racket/format
    syntax/parse
    rilouworld/private/quest/props-meta)
  (only-in rilouworld/quest
           pos size rect world zone
           load-quest))

(define-for-syntax (bundle-path name)
  (string->symbol (~a "rilouworld/bundles/" (syntax->datum name))))

(define-syntax (module-begin stx)
  (syntax-case stx (from use)
    [(_ (from bundle use actor ...) ... expr)
     (with-syntax ([(path ...)
                    (datum->syntax stx
                      (map bundle-path
                           (syntax->list (syntax/loc stx (bundle ...)))))])
       #'(#%module-begin
           (provide object)
           (require (only-in path actor ...)) ...
           (define object expr)))]))

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
                #f ; current-sprites
                ))]))

(define-syntax (parse-zone stx)
  (syntax-parse stx
    #:datum-literals (name map type outside inside rectangles actors)
    [(_ <id>:id
        (~alt (~once (name <name>:str))
              (~optional (map <map>:str)
                         #:defaults ([<map> #'#f]))
              (~optional (type (~and <type> (~or outside inside)))
                         #:defaults ([<type> #'outside]))
              (~once (rectangles <rectangle>:rect-exp ...))
              (~once (actors <actor>:expr ...))) ...)
     #'(zone '<id>
             <name>
             <map>
             '<type>
             (list <rectangle> ...)
             (list <actor> ...))]))
