#lang racket/base

(require
  racket/format
  "engine.rkt"
  "account.rkt")

(define manafia
  (let ()
    (local-require "dimensions/manafia.rkt")
    dimension))
(displayln (~s manafia))

(define options (engine-options 512 384 "Rilouworld"))
(define player (account "Euhmeuh" 'golfia))
(engine-start options manafia player)
