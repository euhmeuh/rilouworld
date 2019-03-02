#lang scribble/manual

@(require
  (for-label
    racket/base
    rilouworld/quest
    rilouworld/bundles/spacecat))

@title{Spacecat Bundle}

@defmodule[rilouworld/bundles/spacecat]

This bundle provides the bare minimum to make a side scrolling shooter world.

@defproc[(player [image path-string?] [pos pos? (pos 0 0)]) actor?]{
  Create the main keyboard-controlled player.
}

@defproc[(star) actor?]{
  
}

@defproc[(troll) actor?]{
  
}
