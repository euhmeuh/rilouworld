#lang racket

(require math)
(provide
  layer%
  make-float-matrix
  display-matrix
  propagate
  train-all)

(define T matrix-transpose)

(define (make-float-matrix 2d-list)
  (list*->matrix 2d-list))

(define (sigmoid x)
  (/ (+ 1.0 (exp (- x)))))

(define (sigmoid-deriv x)
  (let ([y (sigmoid x)])
    (* y (- 1.0 y))))

(define (matrix-sig m)
  (matrix-map sigmoid m))

(define (matrix-dsig m)
  (matrix-map sigmoid-deriv m))

(define (matrix-mul m n)
  (matrix-map * m n))

(define (make-random-matrix rows cols)
  (build-matrix rows cols (lambda (x y) (random))))

(define (make-weights rows cols)
  (matrix-map sub1
    (matrix-scale (make-random-matrix rows cols) 2)))

(define layer%
  (class object%
    (init nb-of-neurons inputs-per-neuron)

    (field [weights (make-weights inputs-per-neuron nb-of-neurons)]
           [outputs (make-float-matrix '[[0.0]])]) ;; we need to remember outputs to calculate error delta later

    (super-new)

    (define/public (process inputs)
      (set! outputs (matrix-sig (matrix* inputs weights)))
      outputs)

    (define/public (update inputs delta)
      (set! weights (matrix+ weights
                             (matrix* (T inputs) delta)))
      weights)))

(define (propagate inputs layers)
  (if (pair? layers)
    (propagate (send (car layers) process inputs) (cdr layers))
    inputs))

(define (backpropagate first-inputs expected-outputs layers)
  (define (get-upper-inputs layers)
    (if (and (pair? layers) (not (empty? (cdr layers))))
      (get-field outputs (cadr layers))
      first-inputs))

  (define (get-delta outputs errors)
    (matrix-mul errors (matrix-dsig outputs)))

  (define (train layers previous-errors)
    (unless (empty? layers)
      (let* ([layer (car layers)]
             [delta (get-delta (get-field outputs layer) previous-errors)]
             [errors (matrix* delta (T (get-field weights layer)))]
             [inputs (get-upper-inputs layers)])
        (send layer update inputs delta)
        (train (cdr layers) errors))))

  (define first-errors (matrix- expected-outputs (get-field outputs (last layers))))

  (train (reverse layers) first-errors))

(define (train-all inputs expected-outputs iterations layers)
  (for ((_ iterations))
    (propagate inputs layers)
    (backpropagate inputs expected-outputs layers)))

(define (display-results matrix)
  (displayln (matrix->list matrix)))

(define (display-matrix matrix)
  (for ((i (in-range (matrix-num-rows matrix))))
    (for ((j (in-range (matrix-num-cols matrix))))
      (printf "~a\t" (matrix-ref matrix i j)))
    (newline)))
