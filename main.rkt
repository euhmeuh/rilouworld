#lang racket/base

(require
  rilouworld/engine
  rilouworld/account
  rilouworld/quest)

(define world (load-quest "worlds/nyancat/index.world"))
(define options (engine-options 512 384 "Rilouworld"))
(define player (account "Euhmeuh" 'golfia))
(engine-start options world player)
