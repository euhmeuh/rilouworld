#lang racket/base

(require
  racket/class
  rilouworld/core/engine
  rilouworld/server/account
  rilouworld/quest/base)

(define world (load-quest "worlds/nyancat/index.world"))
(define options (engine-options 512 384 "Rilouworld"))
(define player (account "Euhmeuh" 'golfia))
(engine-start options world player)
