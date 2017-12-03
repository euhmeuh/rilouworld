#lang racket/base

(require
  racket/class
  "engine.rkt"
  "account.rkt"
  "dimensions/nyancat.rkt")

(define options (engine-options 512 384 "Rilouworld"))
(define player (account "Euhmeuh" 'golfia))
(engine-start options dimension player)
