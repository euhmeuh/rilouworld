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
  (for-template
    racket/base
    rilouworld/private/quest/props)
  syntax/parse)

(define-syntax-class any-exp
  (pattern expr #:with result #'expr))

(define-splicing-syntax-class vec-exp
  (pattern (~seq x y) #:with result #'(vec x y)))

(define-splicing-syntax-class size-exp
  (pattern (~seq x y) #:with result #'(size x y)))

(define-splicing-syntax-class rect-exp
  (pattern (~seq x y w h) #:with result #'(rect x y w h)))

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
