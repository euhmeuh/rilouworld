#lang scribble/manual

@(define (vocab word . content)
  (list
    (tech word)
    " --- "
    (apply deftech #:key word content)))

@(define (vocab-details . content)
  (apply nested #:style 'inset content))

@title{Vocabulary}

This section describes the different terms used throughout Rilouworld's documentation.

@vocab["actor"]{
  An actor is any entity that exists in a given @tech{zone} of a @tech{world}. It can react to @tech{events},
  render to the screen, and emit events to other actors.
}

@vocab-details{
  For example, a @tech{player} is an actor that renders on screen and listens for movement events, and
  a @tech{keyboard} is an invisible actor that listens to pressed and released keys.

  Actors are packed in @tech{bundles}. When you create a @tech{world}, you can use actors from
  the bundles of your choice. Keep in mind though that to stay coherent, you shouldn't pick too much bundles,
  otherwise your world is going to feel out of place and cluttered.

  Good bundles for beginners are: @tt{spacecat}, @tt{ma}.
}

@vocab["bundle"]{
  A collection of @tech{actors} written using the Racket programming language.
  A bundle implements a game logic by providing different actors which are useful together.
}

@vocab["event"]{}

@vocab["keyboard"]{}

@vocab["map"]{}

@vocab["player"]{}

@vocab["world"]{
  A world is a collection of coherent levels made by one or more players, and shared with others.
}

@vocab["zone"]{
  A zone is a playable part of a @tech{world}. It is made of a @tech{map} and any number of @tech{actors}.
}
