(load-from-library "regex.scm")

(define (http:new-session url)
  (let ((easy (curl:easy-init)))
    (curl:setopt easy "CURLOPT_URL" url)
    easy))

(define (http:cleanup easy)
  (curl:cleanup easy))

(define (http:get url)
  (let ((easy (http:new-session url)))
    (curl:perform easy)))

(define (http:perform easy)
  (curl:perform easy))

(define (http:post easy postdata)
  (curl:setopt easy "CURLOPT_POSTFIELDS" postdata)
  (curl:perform easy))

(define (apply-options easy options)
  (if (null? options)
      #t
      (begin (curl:setopt easy (car options) (cadr options))
             (apply-options easy (cddr options)))))

(define (http:basic_auth easy)
  (curl:setopt
    easy
    "CURLOPT_HTTPAUTH"
    (sys:magic-const "CURLAUTH_BASIC")))

(define (http:user:pass easy user pass)
  (curl:setopt easy "CURLOPT_USERPWD" (format #f "~A:~A" user pass)))

(define (http:insecure easy)
  (curl:setopt easy "CURLOPT_SSL_VERIFYHOST" 0)
  (curl:setopt easy "CURLOPT_SSL_VERIFYPEER" 0))

(define (http:get-cookies easy)
  (curl:get-cookies easy))

(define (match-regex rgx items)
  (if (null? items)
      '()
      (let ((match (re-match rgx (car items))))
        (if match
            (car items)
            (match-regex rgx (cdr items))))))

(define (http:get-cookie easy regex)
  (let ((rgx (re-comp regex))
        (cookies (vector->list (curl:get-cookies easy))))
    (match-regex rgx cookies)))
