#lang web-server

(require "anaphora.rkt")

(define-syntax-rule (define-routes list dispatcher url-maker route ...)
  (begin
    (define list '(route ...))
    (define-values (dispatcher url-maker)
      (dispatch-rules route ...))))

(define *database*
  '((dimensions
      ((name "Manafia")
       (ip "1.2.3.4"))
      ((name "Merdelin's World")
       (ip "5.6.7.8")))
    (players
      ((name "EuhMeuh") (hp 100))
      ((name "Maxou") (hp 50))
      ((name "Pit") (hp 12)))))

(define (database key)
  (cdr (assq key *database*)))

(define (find-object predicate lst)
  (findf
    (lambda (obj)
      (let [(field (first predicate))
            (value (third predicate))]
        (aif (assq field obj)
          (string-ci=? (second it) value)
          #f)))
    lst))

(define (serialize obj)
  (response/xexpr
    `(html (head (title "response"))
           (body ,(~s obj)))))

(define-routes routes api-dispatcher api-url
  [("") root]
  [("dimension") dimension]
  [("dimension" (string-arg)) dimension]
  [("player") player]
  [else root])

(define (root req)
  (serialize routes))

(define (dimension req (name #f))
  (serialize (if name
                 (find-object `(name is ,name) (database 'dimensions))
                 (database 'dimensions))))

(define (player req)
  (serialize (database 'players)))

(serve/dispatch api-dispatcher)
