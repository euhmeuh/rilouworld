#lang racket/base

(require
  racket/class
  racket/format
  "engine.rkt"
  "account.rkt")

(define manafia
  (let ()
    (local-require "dimensions/manafia.rkt")
    dimension))
(displayln (~s (get-field data manafia)))

(define options (engine-options 512 384 "Rilouworld"))
(define player (account "Euhmeuh" 'golfia))
(engine-start options manafia player)
