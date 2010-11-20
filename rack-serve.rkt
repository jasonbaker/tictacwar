#lang web-server
(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         web-server/stuffers
         "game.rkt")

(define (start req)
  (main-page req))

(define (render-code-page action-url)
  (let ((prefab '("random")))
    (include-template "input-page.html")))

(define (main-page req)
  (local [(define (response-generator embed/url)
            (list #"text/html" (render-code-page (embed/url handle-post))))
          
          (define (handle-post request)
            (let* ([code (extract-binding/single 'code (request-bindings request))]
                  [result (play-game code)])
              (display result)
               `(html (body (p ,@result)))))]
  (send/suspend/dispatch response-generator)))

(serve/servlet start
               #:command-line? #t
               #:banner? #t
               #:listen-ip #f
               #:port 8000)
