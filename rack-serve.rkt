#lang web-server
(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         web-server/stuffers)

(define (start req)
  (main-page req))

(define (render-code-page action-url)
  (include-template "input-page.html"))

(define (main-page req)
  (local [(define (response-generator embed/url)
            (list #"text/html" (render-code-page (embed/url handle-post))))
          
          (define (handle-post request)
            `(html (body (p ,(extract-binding/single 'code (request-bindings request))))))]
  (send/suspend/dispatch response-generator)))

(serve/servlet start)
