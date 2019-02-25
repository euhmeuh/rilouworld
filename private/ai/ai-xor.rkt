#lang racket

(module+ main

;;; An Artificial Neural Network that learns the rules of binary XOR.

(require "ai.rkt")

(define layer1 (make-object layer% 4 3))
(displayln "layer 1: 4 neurons with 3 inputs:")
(display-matrix (get-field weights layer1))
(define layer2 (make-object layer% 1 4))
(displayln "layer 2: 1 neuron with 4 inputs:")
(display-matrix (get-field weights layer2))
(define network (list layer1 layer2))

;; sample inputs and results for XOR
;; the third column is meaningless, we expect the network to discover that
(define inputs (make-float-matrix '[[0.0 0.0 1.0]
                                    [0.0 1.0 1.0]
                                    [1.0 0.0 1.0]
                                    [0.0 1.0 0.0]
                                    [1.0 0.0 0.0]
                                    [1.0 1.0 1.0]
                                    [0.0 0.0 0.0]]))
(define results (make-float-matrix '[[0.0] [1.0] [1.0] [1.0] [1.0] [0.0] [0.0]]))

;; training
(displayln "training...")
(train-all inputs results 60000 network)

(displayln "layer1 after training:")
(display-matrix (get-field weights layer1))
(displayln "layer 2 after training:")
(display-matrix (get-field weights layer2))

(displayln "results with same inputs as training:")
;; should be similar to the expected results
(display-matrix (propagate inputs network))

(displayln "results with new inputs:")
;; try new inputs not seen before
(display-matrix (propagate (make-float-matrix '[[1.0 1.0 0.0]
                                                [1.0 0.0 1.0]
                                                [1.0 1.0 1.0]
                                                [1.0 0.0 1.0]
                                                [0.0 1.0 1.0]
                                                [0.0 0.0 0.0]
                                                [0.0 1.0 1.0]
                                                [1.0 0.0 1.0]
                                                [1.0 1.0 1.0]])
                            network))
)
