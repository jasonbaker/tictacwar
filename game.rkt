#lang racket
(require racket/sandbox
         "util.rkt"
         "structs.rkt")
(provide play-game board-spaces has-line? turn->string)

(define board-spaces (for*/list ([row "abc"]
                                [col '(1 2 3)])
                               (format "~a~a" row col)))

(define (code->func code)
  (lambda (current-state)
    (let* ([evaluator (make-evaluator 'racket/base `(define current-state ,current-state) #:requires '("./structs.rkt"))]
          [result (evaluator code)])
      (kill-evaluator evaluator)
      result)))
      
(define (take-turn current-player current-state)
  (with-handlers ([exn:fail? (lambda (exn) (raise exn) )])
    (let ([result ((player-func current-player) current-state)])
      (struct-copy state
                   current-state
                   [moves-list (cons (turn (player-name current-player) result) (state-moves-list current-state))]
                   [unused (remq result (state-unused current-state))]))))

(define (has-line? moves)
  (let ((positions (flatten (map string->list moves))))
    (ormap
     (lambda (char)
       (>=
        (count
         (lambda (lchar)
           (char-ci=? lchar char))
         positions)
        3))
     '(#\a #\b #\c #\1 #\2 #\3))))
        
  
(define (winner? player current-state)
  (let ([moves (for/set ((t (state-moves-list current-state))
                         #:when (eq? (turn-player t) (player-name player)))
                        (turn-position t))])
    (if (or
         (subset? (set "a1" "b2" "c3") moves)
         (subset? (set "a3" "b2" "c1") moves))
        #t
        (has-line? (set->list moves)))))
        
  
(define (user-turn current-state)
  (let* ([user (state-user current-state)]
         [new-state (take-turn user current-state)])
    (if (winner? user new-state)
        (struct-copy state new-state [winner user])
        (if (= (length (state-unused new-state)) 0)
            (struct-copy state new-state [winner 'cat])
            (computer-turn new-state)))))

(define (computer-turn current-state)
  (let* ([user (state-computer current-state)]
         [new-state (take-turn user current-state)])
    (if (winner? user new-state)
        (struct-copy state new-state [winner user])
        (user-turn new-state))))
    
(define (play-game x-code o-code)
  (let* ([user (player 'x (code->func x-code))]
        [computer (player 'o (code->func o-code))]
        [initial-state (state board-spaces '() user computer #f)])
    (user-turn initial-state)))
  
(define (turn->string turn-list)
  (map
   (lambda (turn) (format "~a placed at ~a" (turn-player turn) (turn-position turn)))
   turn-list))

(define (random-strategy current-state)
  (let ((unused (state-unused current-state)))
    (list-ref
     unused
     (random (length unused)))))

