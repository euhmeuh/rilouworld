#lang racket/base

(require
  racket/class
  "core/engine.rkt"
  "server/account.rkt"
  "quest/base.rkt")

(define dimension (load-quest "dimensions/nyancat.qst"))
(define options (engine-options 512 384 "Rilouworld"))
(define player (account "Euhmeuh" 'golfia))
(engine-start options dimension player)
