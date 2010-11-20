#lang racket
(require rackunit
         "game.rkt")

(check-true (has-line? '("a1" "a2" "a3")))
(check-true (has-line? '("a3" "b3" "c3")))
(check-false (has-line? '()))

