#lang scribble/manual

@(require
  (for-label
    racket/base
    rilouworld/engine
    rilouworld/quest
    rilouworld/account))

@title{Engine Reference}

@defmodule[rilouworld/engine]

@defproc[(engine-start [options engine-options?] [world world?] [account account?]) any/c]{
  Start the game engine with @racket[options] defining the width, height, and title of the
  window, loading a given local @racket[world] using @racket[account] as the player.
}

@defstruct*[engine-options ([width integer?] [height integer?] [title string?])]{
  Define @racket[width], @racket[height] and @racket[title] of the main game window,
  when passed to @racket[engine-start].
}
