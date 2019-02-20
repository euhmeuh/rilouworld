#lang racket/base

(require
  racket/class
  rilouworld/core/engine
  rilouworld/server/account
  rilouworld/quest/base)

(define dimension (load-quest "dimensions/nyancat.qst"))
(define options (engine-options 512 384 "Rilouworld"))
(define player (account "Euhmeuh" 'golfia))
(engine-start options dimension player)
