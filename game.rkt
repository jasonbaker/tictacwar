#lang racket
(require racket/sandbox)
(provide play-game)

(define (code->func code)
  (lambda (moves-list)
    (let* ([evaluator (make-evaluator 'racket/base `(define moves ',moves-list))]
          [result (evaluator code)])
      (display result)
      (kill-evaluator evaluator)
      result)))
      
(define (take-turn moves-list func current-player other-player)
  (with-handlers ([exn:fail? (lambda (exn) other-player)])
    (let ([result (func moves-list)])
      (cons (list current-player result) moves-list))))
      

(define (play-game my-code [moves-list '()])
  (let ([my-func (code->func my-code)])
    (take-turn moves-list my-func 'user 'computer)))
  
