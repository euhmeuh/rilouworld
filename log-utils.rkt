#lang racket/base

(provide
  ;; log a warning
  warning)

(define (warning sym message . values)
  (displayln (format
               "warning:~a: ~a"
               sym
               (apply format message values))))
