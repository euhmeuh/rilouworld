#lang scribble/manual

@(require
  (for-label
    racket/base
    rilouworld/quest))

@title{Quest Reference}

@defmodulelang[rilouworld/quest]

The Quest language is a DSL for writing game worlds in Rilouworld.
You'll find below the complete description of the language.

For an easy introduction to it, you might want to read @secref["Introduction_to_the_Quest_language"].

@section{The Quest language}

@defmodulelang[rilouworld/quest #:no-declare #:link-target? #f]

@defform[#:literals (name uuid version changelog change date breaking authors author section description zones)
         (world (name world-name)
                (uuid world-uuid)
                (version world-version)
                (description world-desc)
                (changelog change-expr ...)
                (authors author-expr ...)
                (zones zone ...))
         #:grammar
         [(change-expr (change (version change-version) (date change-date) maybe-breaking change-desc))
          (maybe-breaking (breaking))
          (author-expr (author maybe-section author-name))
          (maybe-section (code:line) (section section-name))]
         #:contracts
         [(world-name string?)
          (world-uuid string?)
          (world-version string?)
          (world-desc string?)
          (change-version string?)
          (change-date string?)
          (change-desc string?)
          (author-name string?)
          (section-name string?)
          (zone zone?)]]{
    Create a new playable world.
}

@defform[#:literals (name map type rectangles actors inside outside)
         (zone zone-id
               (name zone-name)
               (map zone-map)
               (type zone-type)
               (rectangles rectangle ...)
               (actors actor ...))
         #:grammar
         [(zone-type (code:line inside) outside)]
         #:contracts
         [(zone-name string?)
          (zone-map path-string?)
          (rectangle rect?)
          (actor actor?)]]{
    Declare a zone of your world with a unique identifier @racket[zone-id] in which actors can exist.
}

@section{Using Quest as a module}

@defmodule[rilouworld/quest #:no-declare #:link-target? #f]

You can require the Quest language as a module in your code in order to manipulate actors,
load, save and verify quest files.

Using Quest as a module will @bold{not} execute the language, generate worlds or actors, it'll just give
you access to useful tools to handle quest files.

@defproc[(load-quest [path path-string?]) (or/c world? zone?)]{}

@defproc[(world? [v any/c]) boolean?]{}
@defproc[(zone? [v any/c]) boolean?]{}
