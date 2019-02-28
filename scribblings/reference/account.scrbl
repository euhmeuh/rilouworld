#lang scribble/manual

@(require
  (for-label
    racket/base
    rilouworld/account))

@title{Account Reference}

@defmodule[rilouworld/account]

@defstruct*[account ([name string?] [zone symbol?])]{
  A local player's account is made of a @racket[name] and a @racket[zone] identifier
  which represents the current zone of the world the player is in.
}
