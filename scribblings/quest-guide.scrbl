#lang scribble/manual

@(require
  (for-label
    racket/base
    racket/contract
    rilouworld/quest))

@title{Introduction to the Quest language}

@defmodulelang[rilouworld/quest #:no-declare #:link-target? #f]

For the full language reference, see @secref["Quest_Reference"].

@section{Creating a world}

The Quest language is a declarative language which provides a simple and intuitive syntax
for writing new adventures in the Rilouworld.

Let's say we want to create a world where the main goal is to take care of plants
and make the most luxurious garden. We will call that the @bold{World of Gardening}!
You can barely restrain from rushing and trying that out, can you?

Well, let's create a new folder @filepath{gardening} and write the @filepath{main.world} file:

@(racketmod #:file "gardening/main.world"
    rilouworld/quest
    (world
      (name "World of Gardening")
      (zones
        (zone garden
          (name "Your Garden")
          (map "garden.map")
          (type outside)
          (rectangles (rect 0 0 100 100))
          (actors (simple-player (pos 50 50)))))))
