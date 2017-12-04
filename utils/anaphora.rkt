#lang racket/base

;;; provide a Scheme hygienic equivalent of the CL anaphoric macros
;;; defined in the book Let Over Lambda by Doug Hoyt

(provide
  ;; create an anaphoric macro
  define-asyntax
  ;; create an anaphoric one rule macro
  define-asyntax-rule
  ;; anaphoric if that binds "it"
  aif
  ;; anaphoric when that binds "it"
  awhen
  ;; anaphoric lambda that binds "self"
  alambda
  ;; anaphoric tree walking that binds "it"
  atree)

(require
  (for-syntax racket/base))

(define-syntax-rule (define-asyntax name (anaphora ...) (pattern template) ...)
  (define-syntax (name stx)
    (syntax-case stx ()
      [pattern
       (with-syntax [(anaphora (datum->syntax stx 'anaphora)) ...]
         (syntax template))] ...)))

(define-syntax-rule (define-asyntax-rule (name pattern ...) (anaphora ...) template)
  (define-asyntax name (anaphora ...)
    [(_ pattern ...) template]))

;; Anaphoric if that binds "it" to the result
;; of the condition
;;
;; Example:
;; (aif (+ 2 20 16 4)
;;   (display it)
;;   (display "Nope"))
;;
(define-asyntax-rule (aif test then else) (it)
  (let ((it test))
    (if it then else)))

;; Anaphoric when that binds "it" to the result
;; of the condition
(define-asyntax-rule (awhen test body ...) (it)
  (let ((it test))
    (when it body ...)))

;; Anaphoric lambda that binds "self" to itself
;; in order to allow recursion
;;
;; Example:
;; (alambda (n)
;;   (if (> n 0)
;;     (cons
;;       n
;;       (self (- n 1)))
;;   '()))
;;
(define-asyntax-rule (alambda params body) (self)
  (let ((self (lambda params body)))
    self))

;; anaphoric tree walking
;;
;; Example:
;; (atree '(1 2 (3 4 (5 6)))
;;        (and (number? it) (even? it))
;;        'even-number)
;;
(define-asyntax-rule (atree tree test result) (it)
  (tree-leaves tree
    (lambda (it) test)
    (lambda (it) result)))

(define (tree-leaves tree test result)
  (if (and (list? tree) (not (null? tree)))
    (cons
      (tree-leaves (car tree) test result)
      (tree-leaves (cdr tree) test result))
    (if (test tree)
      (result tree)
    tree)))
