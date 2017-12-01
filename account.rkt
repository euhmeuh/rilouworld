#lang racket/base

(provide
  ;; informations and progression of a player
  (struct-out account))

(struct account (name zone))
