#lang scribble/manual

@(require
  (for-label
    racket/base
    racket/contract
    rilouworld/engine
    rilouworld/quest
    rilouworld/account))

@title{Rilouworld: Decentralized Game Universe}
@author{Jérôme Martin}

@section{Overview}

Rilouworld is a decentralized game environment in which players can create and share
their own worlds.

Worlds are made using a domain specific language call Quest, which allows you to describe
zones, monsters, objects, weapons, characters, dialogs, and of course, quests.

This project is a work in progress and an experimentation.
It's not ready for production yet, but you are welcome to join us on the
@hyperlink["https://github.com/euhmeuh/rilouworld"]{Github repository} if you like the idea!

@section{Quick start}

@defmodule[rilouworld/engine]

@(racketblock
  (engine-start
   (engine-options 512 384 "Rilouworld")
   (load-quest "worlds/ma/index.world")
   (account "EuhMeuh" 'supa)))

@defproc[(engine-start [options engine-options?] [world world?] [account account?]) any/c]{
  Start the game engine with @racket[options] defining the width, height, and title of the
  window, loading a given local @racket[world] using @racket[account] as the player.
}

@defstruct*[engine-options ([width integer?] [height integer?] [title string?])]{
  Define @racket[width], @racket[height] and @racket[title] of the main game window,
  when passed to @racket[engine-start].
}

@include-section["networking.scrbl"]
@include-section["quest.scrbl"]
