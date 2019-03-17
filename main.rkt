#lang racket/base

(require
  rilouworld/engine
  rilouworld/account
  rilouworld/quest)

(module+ main
  (define world (load-quest "worlds/spacecat/main.world"))
  (define options (engine-options 512 384 "Rilouworld"))
  (define player (account "Euhmeuh" 'golfia))
  (engine-start options world player))

(module+ test)
