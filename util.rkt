#lang racket
(provide set->list)
(define (set->list inset)
  (set-map inset (lambda (x) x)))
