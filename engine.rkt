#lang racket

(require
  mode-lambda
  mode-lambda/static
  lux
  lux/chaos/gui
  lux/chaos/gui/key
  (prefix-in gl: mode-lambda/backend/gl))

(define *label* "Rilouworld")
(define *width* 512)
(define *height* 384)
(define *width/2* (/ 512 2.))
(define *height/2* (/ 384 2.))
(define *nb-of-layers* 8)

(define (prepare-render-context)
  (define sprite-db (make-sprite-db))
  (add-palette!/file sprite-db 'palette (build-path "." "palette.png"))
  (add-sprite!/file sprite-db 'nyancat (build-path "." "nyancat.png") #:palette 'palette)
  (define compiled-sprite-db (compile-sprite-db sprite-db))
  (values
    (gl:stage-draw/dc compiled-sprite-db
                      *width*
                      *height*
                      *nb-of-layers*)
    compiled-sprite-db))

(define (prepare-layer-config)
  (vector (layer *width/2* *height/2*)))

(define (make-renderer render-context sprite-db)
  (define static-states '())
  (define dynamic-states
    (lambda ()
      (list (sprite *width/2* *height/2*
                    (sprite-idx sprite-db 'nyancat)
                    #:layer 0
                    #:pal-idx (palette-idx sprite-db 'palette)))))
  (lambda ()
    (render-context (prepare-layer-config) static-states (dynamic-states))))

(struct game (render-context renderer)
  #:methods gen:word
  [(define (word-fps word) 60.0)
   (define (word-label word frame-time)
     (lux-standard-label *label* frame-time))
   (define (word-output word)
     ((game-renderer word)))
   (define (word-event word event)
     (cond
       [(or (eq? event 'close)
            (and (key-event? event)
                 (eq? 'escape (key-event-code event))))
        #f]
       [else word]))
   (define (word-tick word)
     word)])

(call-with-chaos
  (make-gui #:mode gl:gui-mode)
  (lambda ()
    (define-values (render-context sprite-db) (prepare-render-context))
    (fiat-lux (game render-context
                    (make-renderer render-context sprite-db)))))
