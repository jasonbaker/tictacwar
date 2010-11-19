#lang racket
(require racket/sandbox)
(provide play-game board-spaces)

(struct turn (player position))
(struct player (name func))
(struct state (unused moves-list user computer))

(define board-spaces (for*/list ([row "abc"]
                                [col '(1 2 3)])
                               (format "~a~a" row col)))

(define (code->func code)
  (lambda (moves-list)
    (let* ([evaluator (make-evaluator 'racket/base `(define moves ',moves-list))]
          [result (evaluator code)])
      (display result)
      (kill-evaluator evaluator)
      result)))
      
(define (take-turn current-player current-state)
  (with-handlers ([exn:fail? (lambda (exn) other-player)])
    (let ([result (func (state-moves-list current-state))])
      (struct-copy state
                   current-state
                   [moves-list (cons (list current-player result) moves-list)])))

(define (player-turn current-state)
  

(define (play-game my-code [moves-list '()])
  (let ([user (player 'x (code->func my-code))]
        [computer (player 'o '())]
        [initial-state (state board-spaces '() user computer)])
    (take-turn initial-state)))
  
(define (turn->str turn-list)
  (map
   (lambda (turn) (format "~a placed at ~a" (turn-player turn) (turn-position turn)))))
  
