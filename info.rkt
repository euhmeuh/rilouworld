#lang info

(define pkg-desc "The decentralized game universe")
(define pkg-authors '(euhmeuh))
(define version "0.1.0")

(define scribblings '(("scribblings/rilouworld.scrbl" (multi-page) (gui-library))))

(define deps
  (list
    "base"
    "math-lib"
    "draw-lib"
    "anaphoric"
    "web-server-lib" ;; TODO: replace with "web-galaxy"
    "mode-lambda"
    "lux"
    "reprovide-lang"))

(define build-deps
  (list
    "racket-doc"
    "scribble-lib"))
