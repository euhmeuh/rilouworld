#lang racket/base

;;; display the graphics on the screen
;;; and handle inputs

(provide
  ;; open a window and start the main game loop, handling inputs and displaying graphics
  engine-start
  ;; engine initial configuration
  (struct-out engine-options))

(require
  racket/class
  racket/function
  mode-lambda
  mode-lambda/static
  lux
  lux/chaos/gui
  (prefix-in gl: mode-lambda/backend/gl)
  "../utils/log.rkt"
  "../utils/struct.rkt"
  "../utils/anaphora.rkt"
  (prefix-in quest: "../quest/entities.rkt"))

(define *nb-of-layers* 8)
(define *fps* 60.0)
(define current-frame (make-parameter 0))

(struct loop (options dimension renderer)
  #:methods gen:word
  [(define (word-fps self) *fps*)
   (define (word-label self frame-time)
     (lux-standard-label (engine-options-title (loop-options self)) frame-time))
   (define (word-output self)
     ((loop-renderer self)))
   (define (word-event self event)
     (send (loop-dimension self) handle-event self event))
   (define (word-tick self)
     (send (loop-dimension self) emit 'tick)
     (current-frame (add1 (current-frame)))
     self)])

(struct engine-options (width height title))

(define (sprite-ref name index)
  (string->symbol (format "~a/~a" name index)))

(define (make-database resources)
  (define sprite-db (make-sprite-db))
  (add-palette!/file sprite-db 'palette (build-path "." "dimensions" "palette.png"))
  (for-each
    (lambda (resource)
      (cond
        [(quest:animation? resource) (handle-animation resource sprite-db)]
        [else (handle-resource resource sprite-db)]))
    resources)
  (compile-sprite-db sprite-db))

(define (handle-resource resource sprite-db)
  (with-values resource (name path) from quest:resource
    (set! path (build-path "." "dimensions" path))
    (if (file-exists? path)
        (add-sprite!/file sprite-db
                          name
                          path
                          #:palette 'palette)
        (warning 'sprite-build
                 "Could not find file '~a' for resource '~a'." path name))))

(define (handle-animation resource sprite-db)
  (with-values resource (name path) from quest:resource
    (with-values resource (size frames) from quest:animation
      (set! path (build-path "." "dimensions" path))
      (aif (open-bitmap path)
           (for ([bitmap (cut-image-in-parts it size frames)]
                 [index (in-naturals 0)])
             (add-sprite!/bm sprite-db
                             (sprite-ref name index)
                             (thunk bitmap)
                             #:palette 'palette))
           (warning 'sprite-build
                    "Could not find file '~a' for resource '~a'." path name)))))

(define (open-bitmap path)
  (local-require racket/draw)
  (and (file-exists? path)
       (read-bitmap path)))

(define (cut-image-in-parts bitmap size frames)
  (list bitmap))

(define (make-render-context width height sprite-db)
  (gl:stage-draw/dc sprite-db width height *nb-of-layers*))

(define (make-layer-config options)
  (with-values options (width height) from engine-options
    (vector (layer (/ width 2.) (/ height 2.)))))

(define (make-renderer options render-context sprite-db dimension)
  (define static-states
    (thunk
      (map (curryr render-static-sprite sprite-db)
           (send dimension collect-static-sprites))))
  (define dynamic-states
    (thunk
      (map (curryr render-sprite sprite-db)
           (send dimension collect-dynamic-sprites))))
  (thunk
    (render-context (make-layer-config options)
                    (static-states)
                    (dynamic-states))))

(define (render-sprite the-sprite sprite-db)
  (with-values the-sprite (pos resource layer) from quest:sprite
    (sprite (quest:pos-x pos) (quest:pos-y pos)
            (sprite-idx sprite-db (find-current-sprite resource))
            #:layer layer
            #:pal-idx (palette-idx sprite-db 'palette))))

(define (render-static-sprite sprite sprite-db)
  #f)

(define (find-current-sprite resource)
  (cond
    [(quest:animation? resource)
     (sprite-ref (quest:resource-name resource) 0)]
    [else (quest:resource-name resource)]))

(define (make-loop options dimension account)
  (define sprite-db
    (make-database (get-field resources dimension)))
  (define render-context
    (with-values options (width height) from engine-options
      (make-render-context width height sprite-db)))
  (define renderer
    (make-renderer options render-context sprite-db dimension))
  (loop options dimension renderer))

(define (engine-start options dimension account)
  (call-with-chaos
    (make-gui #:mode gl:gui-mode)
    (thunk
      (fiat-lux (make-loop options dimension account)))))
