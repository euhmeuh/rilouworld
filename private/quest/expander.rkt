#lang racket/base

;;; the Quest language is a DSL for writing Rilouworlds

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         pos
         size
         rect
         (rename-out [parse-world world])
         (rename-out [parse-zone zone])
         (rename-out [load-quest ->]))

(require
  syntax/parse
  (for-syntax
    racket/base
    racket/format
    syntax/parse
    syntax/stx)
  rilouworld/private/quest/actors
  rilouworld/private/quest/loader)

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

(define (pos x y)
  (pos x y))

(define (size x y)
  (size x y))

(define (rect x y w h)
  (rect x y w h))

(begin-for-syntax
  (define-syntax-class pos-exp #:literals (pos) (pattern (pos x y)))
  (define-syntax-class size-exp #:literals (size) (pattern (size w h)))
  (define-syntax-class rect-exp #:literals (rect) (pattern (rect x y w h)))

  (define-syntax-class change-exp
    #:datum-literals (change version date breaking)
    (pattern (change (version <version>:str)
                     (date <date>:str)
                     (~optional (~and (breaking) breaking-mark))
                     <desc>:str)
             #:with <breaking?> (if (attribute breaking-mark) #'#t #'#f)
             #:with result #'(change <version> <date> <breaking?> <desc>)))

  (define-syntax-class author-exp
    #:datum-literals (author section)
    (pattern (author (~optional (section <section>:str)
                                #:defaults ([<section> #'#f]))
                     <name>:str)
             #:with result #'(author <name> <section>)))

  (define-syntax-class resource-exp
    #:datum-literals (image animation durations)
    (pattern (image <id>:id
                    <path>:str
                    (~optional <hitbox>:rect-exp
                               #:defaults ([<hitbox> #'#f])))
             #:with result #'(image '<id> <path> <hitbox>))
    (pattern (animation <id>:id
                        <path>:str
                        (~optional <hitbox>:rect-exp
                                   #:defaults ([<hitbox> #'#f]))
                        <size>:size-exp
                        (durations <duration>:nat ...))
             #:with <length> (datum->syntax this-syntax (length (syntax->datum #'(<duration> ...))))
             #:with result #'(animation '<id> <path> <hitbox> <size> '(<duration> ...) <length> 0 0))))

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
     #`(world <name>
              <uuid>
              <version>
              <desc>
              (list #,@(if (attribute <change>) #'(<change>.result ...) #'()))
              (list #,@(if (attribute <author>) #'(<author>.result ...) #'()))
              (list #,@(if (attribute <res>) #'(<res>.result ...) #'()))
              (list <zone> ...))]))

(define-syntax (parse-zone stx)
  (syntax-parse stx
    #:datum-literals (name map type outside inside rectangles actors)
    [(_ zone-id:id
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
