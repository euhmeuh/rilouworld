#lang racket/base

;;; Basic procedures to manipulate quest actors

(provide
  load-quest)

(require
  racket/list
  racket/contract)

(define (handle-load-error err)
  (if (exn:srclocs? err)
    (let* ([get-srcloc (exn:srclocs-accessor err)]
           [path (srcloc-source (first (get-srcloc err)))])
      (raise-user-error 'quest
                        "error while loading quest '~a'.\n~a"
                        path
                        (exn-message err)))
    (raise-user-error 'quest
                      "error while loading a quest:\n~a"
                      (exn-message err))))

(define (load-quest path)
  (with-handlers ([(or/c exn:fail:contract?
                         exn:fail:syntax?) handle-load-error])
    (dynamic-require path 'object)))
