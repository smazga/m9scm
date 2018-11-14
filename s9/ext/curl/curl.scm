(curl:load-consts)

(define (http:new-session url)
  (let ((easy (curl:easy-init)))
    (curl:setopt easy curl:CURLOPT_URL url)
    easy))

(define (http:cleanup easy)
  (curl:cleanup easy))

(define (http:get url)
  (let ((easy (http:new-session url)))
    (curl:perform easy)))

(define (http:perform easy)
  (curl:perform easy))

(define (http:post easy postdata)
  (curl:setopt easy curl:CURLOPT_POSTFIELDS postdata)
  (curl:perform easy))

(define (apply-options easy options)
  (if (null? options)
      #t
      (begin (curl:setopt easy (car options) (cadr options))
             (apply-options easy (cddr options)))))

(define (http:basic_auth easy)
  (curl:setopt
    easy
    curl:CURLOPT_HTTPAUTH
    curl:CURLAUTH_BASIC))

(define (http:user:pass easy user pass)
  (curl:setopt easy curl:CURLOPT_USERPWD (format #f "~A:~A" user pass)))

(define (http:insecure easy)
  (curl:setopt easy curl:CURLOPT_SSL_VERIFYHOST 0)
  (curl:setopt easy curl:CURLOPT_SSL_VERIFYPEER 0))

(define (http:get-cookies easy)
  (curl:get-cookies easy))
