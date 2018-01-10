#lang racket/base

;;; the Quest language is a DSL for writing Rilouworld's dimensions

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out (module-begin #%module-begin))
         
         ;; provide the base entities
         (rename-out (make-pos pos))
         (rename-out (make-size size))
         (rename-out (make-rect rect))
         (rename-out (make-scrolling-bg scrolling-bg))
         (rename-out (make-spawner spawner))
         (rename-out (make-particle particle))
         (rename-out (make-resources resources))
         (rename-out (make-image image))
         (rename-out (make-animation animation))
         (rename-out (make-zone zone))
         (rename-out (make-dimension dimension))
         )

(require
  racket/class
  (for-syntax racket/base)
  "../utils/anaphora.rkt"
  "entities.rkt")

(define-for-syntax (package-path name)
  (path->string (build-path ".." "packages" (format "~a.rkt" (syntax->datum name)))))

(define-syntax (module-begin stx)
  (syntax-case stx (from use)
    [(_ (from package use entity ...) ... expr)
     (with-syntax ([(path ...)
                    (datum->syntax stx
                      (map package-path
                           (syntax->list (syntax/loc stx (package ...)))))])
       #'(#%module-begin
           (provide object)
           (require (only-in path entity ...)) ...
           (define object expr)))]))

(define (make-pos x y)
  (pos x y))

(define (make-size x y)
  (size x y))

(define (make-rect x y w h)
  (rect x y w h))

(define (make-scrolling-bg image direction speed)
  (scrolling-bg image direction speed))

(define-asyntax-rule (make-spawner rect (freq constructor arg ...) ...) (<pos>)
  (let ([<pos> '<pos>])
    (spawner rect (list (spawn-info freq constructor (list arg ...)) ...))))

(define (make-particle image pos direction lifetime)
  (particle image pos direction lifetime))

(define-syntax-rule (make-resources resource ...)
  (list resource ...))

(define (make-image name path [hitbox (rect 0 0 0 0)])
  (image name path hitbox))

(define (make-animation name path size frames [hitbox (rect 0 0 0 0)])
  (animation name path hitbox size frames (length frames) 0 0))

(define (make-zone name title size . entities)
  (zone name title size entities))

(define (make-dimension title sprites . zones)
  (make-object dimension% 'title sprites zones))
