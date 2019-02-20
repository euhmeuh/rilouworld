#lang racket/base

(provide
  sprite-ref
  make-database)

(require
  racket/class
  racket/function
  anaphoric
  mode-lambda
  mode-lambda/static
  rilouworld/utils/log
  rilouworld/utils/struct
  (prefix-in quest: rilouworld/quest/entities))

(define (sprite-ref name index)
  (string->symbol (format "~a/~a" name index)))

(define (make-database resources)
  (define sprite-db (make-sprite-db))
  (add-palette!/file sprite-db 'palette (build-path "." "worlds" "palette.png"))
  (for-each
    (lambda (resource)
      (cond
        [(quest:animation? resource) (handle-animation resource sprite-db)]
        [(quest:image? resource) (handle-image resource sprite-db)]
        [else (void)]))
    resources)
  (compile-sprite-db sprite-db))

(define (handle-image image sprite-db)
  (with-values image (name path) from quest:resource
    (set! path (build-path "." "worlds" path))
    (if (file-exists? path)
        (add-sprite!/file sprite-db
                          name
                          path
                          #:palette 'palette)
        (warning 'sprite-build
                 "Could not find file '~a' for image '~a'." path name))))

(define (handle-animation animation sprite-db)
  (with-values animation (name path) from quest:resource
    (with-values animation (size) from quest:animation
      (set! path (build-path "." "worlds" path))
      (aif (open-bitmap path)
           (for ([bitmap (cut-image-in-parts it size)]
                 [index (in-naturals)])
             (add-sprite!/bm sprite-db
                             (sprite-ref name index)
                             (thunk bitmap)
                             #:palette 'palette))
           (warning 'sprite-build
                    "Could not find file '~a' for animation '~a'." path name)))))

(define (open-bitmap path)
  (local-require racket/draw)
  (and (file-exists? path)
       (read-bitmap path)))

(define (cut-image-in-parts bitmap size)
  (local-require racket/draw)
  (with-values size (w h) from quest:size
    (define-values (bm-w bm-h) (bitmap-size bitmap))
    (define-values (tiles-w tiles-h) (values (quotient bm-w w)
                                             (quotient bm-h h)))
    (for/list ([x (in-range 0 (* tiles-w tiles-h))])
      (cut-image bitmap
                 (* (remainder x tiles-w) w)
                 (* (quotient x tiles-w) h)
                 w
                 h))))

(define (bitmap-size bitmap)
  (local-require racket/draw)
  (values (send bitmap get-width)
          (send bitmap get-height)))

(define (cut-image bitmap x y w h)
  (local-require racket/draw)
  (let* ([result (make-bitmap w h)]
         [dc (new bitmap-dc% [bitmap result])])
    (send dc draw-bitmap-section bitmap 0 0 x y w h)
    result))
