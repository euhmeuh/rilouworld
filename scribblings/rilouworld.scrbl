#lang scribble/manual

@(require
  (for-label
    racket/base
    racket/contract
    rilouworld/engine
    rilouworld/quest
    rilouworld/account))

@title[#:style 'toc]{Rilouworld: Decentralized Game Universe}
@author{Jérôme Martin}

Rilouworld is a decentralized game environment in which players can create and share their own worlds.

@(local-table-of-contents)

@section{Overview}

Rilouworld is a decentralized game environment in which players can create and share
their own worlds.

Worlds are made using a domain specific language call @bold{Quest} (@racketmodname[rilouworld/quest]),
which allows you to describe zones, monsters, objects, weapons, characters, dialogs, and of course, quests.

This project is a work in progress and an experimentation.
It's not ready for production yet, but you are welcome to join us on the
@hyperlink["https://github.com/euhmeuh/rilouworld"]{Github repository} if you like the idea!

@subsection{Starting the engine}

@defmodule*[(rilouworld/engine rilouworld/quest rilouworld/account) #:no-declare #:link-target? #f]

To start the game engine as a client, you can use the @racket[engine-start] function:

@(racketblock
  (engine-start
   (engine-options 512 384 "Rilouworld")
   (load-quest "worlds/ma/index.world")
   (account "EuhMeuh" 'supa)))

@include-section["quest-guide.scrbl"]
@include-section["networking-guide.scrbl"]
@include-section["bundles/bundles.scrbl"]
@include-section["reference/reference.scrbl"]
@include-section["vocabulary.scrbl"]
