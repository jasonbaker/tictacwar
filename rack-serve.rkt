#lang web-server
(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         web-server/stuffers
         "game.rkt"
         "structs.rkt")

(define (start req)
  (main-page req))

;; Render the first page template
(define (render-code-page action-url)
  (let ((prefab '("random" "error")))
    (include-template "input-page.html")))

;; Determine the winner in a given state
(define (extract-winner final-state)
  (symbol->string
   (if (eq? (state-winner final-state) 'cat)
       'cat
       (player-name (state-winner final-state)))))

;; Make a hash table representing the board
(define (make-board-hash final-state)
  (let ([moved-hash (make-hash
                    (for/list ([t (state-moves-list final-state)]
                               #:when (turn-position t))
                              `(,(turn-position t) . ,(symbol->string (turn-player t)))))])
    (for ([p (state-unused final-state)])
         (dict-set! moved-hash p " "))
    (make-immutable-hash (hash->list moved-hash))))

;; Render the results page template
(define (render-result-page final-state)
  (let* ([winner (extract-winner final-state)]
         [turns (turn-list->string-list (reverse (state-moves-list final-state)))]
         [b (curry hash-ref (make-board-hash final-state))])
    (include-template "result-page.html")))
       
;; The servelet's main entry point
(define (main-page req)
  (local [(define (response-generator embed/url)
            (list #"text/html" (render-code-page (embed/url handle-post))))
          
          (define (handle-post request)
            (let* ([x-code (extract-binding/single 'codex (request-bindings request))]
                   [o-code (extract-binding/single 'codeo (request-bindings request))]
                   [result (play-game x-code o-code)])
               (list #"text/html" (render-result-page result))))]
  (send/suspend/dispatch response-generator)))

(serve/servlet start
               #:command-line? #t
               #:banner? #t
               #:listen-ip #f
               #:port 8000
               #:servlet-current-directory ".")
