#lang racket/base

(provide
  any-exp
  vec-exp
  size-exp
  rect-exp
  change-exp
  author-exp
  resource-exp)

(require
  (for-syntax
    racket/base
    syntax/parse)
  (for-template
    racket/base
    rilouworld/private/quest/props)
  syntax/parse)

(define-syntax-class any-exp
  (pattern _ #:with result this-syntax))

(define-syntax (define-prop-shortcut stx)
  (syntax-parse stx
    [(_ <class> (<id> <arg> ...))
     #'(define-splicing-syntax-class <class>
         (pattern (~seq ((~literal <id>) <arg> ...))
                  #:with result #'(<id> <arg> ...))
         (pattern (~seq <arg> ...)
                  #:with result #'(<id> <arg> ...)))]))

(define-prop-shortcut vec-exp (vec x y))
(define-prop-shortcut size-exp (size x y))
(define-prop-shortcut rect-exp (rect x y w h))

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
  #:datum-literals (image animation rect durations)
  (pattern (image <id>:id
                  <path>:str
                  (~optional (rect <hitbox>:rect-exp)
                             #:defaults ([<hitbox>.result #'#f])))
           #:with result #'(image '<id> <path> <hitbox>.result))
  (pattern (animation <id>:id
                      <path>:str
                      (~optional (rect <hitbox>:rect-exp)
                                 #:defaults ([<hitbox>.result #'#f]))
                      (size <size>:size-exp)
                      (durations <duration>:nat ...))
           #:with <length> (datum->syntax this-syntax (length (syntax->datum #'(<duration> ...))))
           #:with result #'(animation '<id> <path> <hitbox>.result <size>.result '(<duration> ...) <length> 0 0)))
