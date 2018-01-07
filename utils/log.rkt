#lang racket/base

(provide
  ;; output a value and returns it unchanged
  dvalue
    ;; log a warning
  warning)

(define (dvalue value)
  (displayln value)
  value)

(define (warning sym message . values)
  (displayln (format
               "warning:~a: ~a"
               sym
               (apply format message values))))
