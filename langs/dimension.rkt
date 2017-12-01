#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out (module-begin #%module-begin)))

(require
  racket/class
  lux/chaos/gui/key)

(define-syntax-rule (module-begin expr ...)
  (#%module-begin
    (provide dimension)
    (define dimension (make-object dimension% (quote expr ...)))))

(define dimension%
  (class object%
    (init-field data)

    (define/private (emit event)
      (displayln (format "Key ~s" (key-event-code event)))
      #t)

    (define/public (handle-event word event)
      (cond
       [(or (eq? event 'close)
            (and (key-event? event)
                 (eq? 'escape (key-event-code event))))
        #f]
       [(key-event? event)
        (if (emit event)
            word
            #f)]
       [else word]))

    (super-new)))
