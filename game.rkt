#lang racket
(require racket/sandbox
         "util.rkt"
         "structs.rkt")
(provide play-game board-spaces has-line? turn->string)

(define return-final-state (make-parameter #f))

(define board-spaces (for*/list ([row "abc"]
                                [col '(1 2 3)])
                               (format "~a~a" row col)))

(define (code->func code)
  (parameterize ([sandbox-eval-limits '(1 10)])
    (let ([evaluator (make-evaluator 'racket/base #:requires '("./structs.rkt"))])
      (lambda (current-state)
        (evaluator `(define current-state ,current-state))
        (evaluator code)))))

(define (take-turn current-player current-state)
  (let ([result ((player-func current-player) current-state)])
    (if (member result (state-unused current-state))
        (struct-copy state
                     current-state
                     [moves-list (cons (turn (player-name current-player) result #f) (state-moves-list current-state))]
                     [unused (remove result (state-unused current-state))])
        (error (format "board space \"~a\" is already taken or does not exist" result)))))

;; Determine if there is a vertical or horizontal row.
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
        
  
;; Has player won?
(define (winner? player current-state)
  (let ([moves (for/set ((t (state-moves-list current-state))
                         #:when (eq? (turn-player t) (player-name player)))
                        (turn-position t))])
    (if (or
         (subset? (set "a1" "b2" "c3") moves)
         (subset? (set "a3" "b2" "c1") moves))
        #t
        (has-line? (set->list moves)))))
        
(define (sandbox-was-terminated-for? exn symbol)
  (and (exn:fail:resource? exn) (eq? (exn:fail:resource-resource exn) symbol)))
      

(define (make-exception-handler current-state other-player current-player)
  (lambda (exn)
    (let ([message #f]
          [current-player-name (player-name current-player)])
      (cond
       [(sandbox-was-terminated-for? exn 'time)
        (set! message (format "Player ~a took too long" current-player-name))]
      [(sandbox-was-terminated-for? exn 'memory)
       (set! message (format "Player ~a used too much memory" current-player-name))]
      ;; The sandbox was killed for some other reason (possibly accessing the file system or network)
      [(exn:fail:sandbox-terminated? exn)
       (set! message (format "Player ~a did something they weren't supposed to" current-player-name))]
      [else
       (set! message (format "Player ~a raised an exception:  ~a" current-player-name (exn-message exn)))])
      ((return-final-state)
       (struct-copy state
                    current-state
                    [winner other-player]
                    [moves-list (cons (turn #f #f message)
                                      (state-moves-list current-state))])))))

(define (x-turn current-state)
  (with-handlers ([exn:fail? (make-exception-handler current-state (state-o current-state) (state-x current-state))])
    (let* ([x (state-x current-state)]
           [new-state (take-turn x current-state)])
      (if (winner? x new-state)
          (struct-copy state new-state [winner x])
          (if (= (length (state-unused new-state)) 0)
              (struct-copy state new-state [winner 'cat])
              (o-turn new-state))))))

(define (o-turn current-state)
  (with-handlers ([exn:fail? (make-exception-handler current-state (state-x current-state) (state-o current-state))])
    (let* ([o (state-o current-state)]
           [new-state (take-turn o current-state)])
      (if (winner? o new-state)
          (struct-copy state new-state [winner o])
          (x-turn new-state)))))
    
(define (play-game x-code o-code)
  (let* ([x (player 'x (code->func x-code))]
         [o (player 'o (code->func o-code))]
         [initial-state (state board-spaces '() x o #f)])
    (let/cc return
            (parameterize ((return-final-state return))
              (x-turn initial-state)))))
  
(define (turn->string turn-list)
  (map
   (lambda (turn)
     (if (turn-message turn)
         (turn-message turn)
         (format "~a placed at ~a" (turn-player turn) (turn-position turn))))
   turn-list))

(define (random-strategy current-state)
  (let ((unused (state-unused current-state)))
    (list-ref
     unused
     (random (length unused)))))

