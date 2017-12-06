#lang racket/base

;;; the Quest language is a DSL for writing Rilouworld's dimensions

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out (module-begin #%module-begin))
         
         ;; provide the base entities
         (rename-out (make-pos pos))
         (rename-out (make-rect rect))
         (rename-out (make-scrolling-bg scrolling-bg))
         (rename-out (make-resources resources))
         (rename-out (make-zone zone))
         (rename-out (make-spawner spawner))
         (rename-out (make-dimension dimension))
         )

(require
  racket/class
  (for-syntax racket/base)
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

(define (make-rect x y w h)
  (rect x y w h))

(define (make-scrolling-bg resource direction speed)
  (scrolling-bg resource direction speed))

(define (make-dimension title sprites . zones)
  (make-object dimension% 'title sprites zones))

(define-syntax-rule (make-resources (name path body ...) ...)
  (list (make-resource 'name path body ...) ...))

(define (make-resource name path [hitbox (rect 0 0 0 0)])
  (resource name path hitbox))

(define (make-zone name title rect . objects)
  (zone name title rect objects))

(define (make-spawner rect . objects)
  (spawner rect objects))
