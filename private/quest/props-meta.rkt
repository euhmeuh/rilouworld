#lang racket/base

(provide
  pos-exp
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
           #:with result #'(animation '<id> <path> <hitbox> <size> '(<duration> ...) <length> 0 0)))
