#lang racket

(provide anaphoric-macro aif awhen alambda atree)

(define-syntax anaphoric-macro
  (syntax-rules ()
    ((_ name (anaphora ...) (pattern template) ...)
     (define-syntax name
       (lambda (x)
         (syntax-case x ()
           (pattern
            (with-syntax ((anaphora (datum->syntax x 'anaphora)) ...)
              (syntax template))) ...))))))

;; Anaphoric if that binds "it" to the result
;; of the condition
;;
;; Example:
;; (aif (+ 2 20 16 4)
;;   (display it)
;;   (display "Nope"))
;;
(anaphoric-macro aif (it)
  ((_ test then else)
   (let ((it test)) (if it then else))))

;; Anaphoric when that binds "it" to the result
;; of the condition
(anaphoric-macro awhen (it)
  ((_ test body ...)
   (let ((it test)) (when it body ...))))

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
(anaphoric-macro alambda (self)
  ((_ params body)
   (letrec ((self (lambda params body)))
     self)))

;; anaphoric tree walking
;;
;; Example:
;; (tree-walk '(1 2 (3 4 (5 6)))
;;            (and (number? it) (even? it))
;;            'even-number)
;;
(anaphoric-macro atree (it)
  ((_ tree test result)
   (tree-leaves tree
     (lambda (it)
       test)
     (lambda (it)
       result))))

(define (tree-leaves tree test result)
    (if (and (list? tree) (not (null? tree)))
      (cons
        (tree-leaves (car tree) test result)
        (tree-leaves (cdr tree) test result))
      (if (test tree)
        (result tree)
      tree)))
