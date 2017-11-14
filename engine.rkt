#lang racket

(require
  racket/gui/base
  sgl
  sgl/gl
  sgl/gl-vectors)

(define frame (new frame%
                   [label "Final Experience"]
                   [width 512]
                   [height 384]))

(define (painter canvas dc)
  (send canvas with-gl-context
    (lambda ()
      (gl-begin 'triangles)
      (gl-vertex 1 2 3)
      (gl-vertex-v (gl-float-vector 1 2 3 4))
      (gl-end)
      (gl-flush)
      (send canvas swap-gl-buffers)
      )))

(define cfg (new gl-config%))

(define canvas (new canvas%
                    [parent frame]
                    [paint-callback painter]
                    [style '(gl no-autoclear)]
                    [gl-config cfg]))

(send frame show #t)
