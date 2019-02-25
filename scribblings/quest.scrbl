#lang scribble/manual

@(require
  (for-label
    racket/base
    racket/contract
    rilouworld/quest))

@title{The Quest language}

@defmodulelang[rilouworld/quest]

@defproc[(load-quest [path path-string?]) (or/c world? zone?)]{}

@defproc[(world? [v any/c]) boolean?]{}
@defproc[(zone? [v any/c]) boolean?]{}
