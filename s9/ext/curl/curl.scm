(define (http:get url . options)
  (let ((easy (curl:easy-init)))
    (curl:setopt easy "CURLOPT_URL" url)
    (apply-options easy (flatten options))
    (let ((response (curl:perform easy)))
      (curl:cleanup easy)
      response)))

(define (http:post url postdata . options)
  (let ((easy (curl:easy-init)))
    (curl:setopt easy "CURLOPT_URL" url)
    (curl:setopt easy "CURLOPT_POSTFIELDS" postdata)
    (apply-options easy (flatten options))
    (let ((response (curl:perform easy)))
      (curl:cleanup easy)
      response)))

(define (apply-options easy options)
  (if (null? options) #t
      (begin
	(curl:setopt easy (car options) (cadr options))
	(apply-options easy (cddr options)))))

(define (http:enable_cookies file)
  '("CURLOPT_COOKIEFILE" . file))

(define (http:basic_auth)
  (cons "CURLOPT_HTTPAUTH" (sys:magic-const "CURLAUTH_BASIC")))

(define (http:user:pass user pass)
  (cons "CURLOPT_USERPWD" (format #f "~A:~A" user pass)))

(define (http:insecure)
  '(("CURLOPT_SSL_VERIFYHOST" . 0)
    ("CURLOPT_SSL_VERIFYPEER" . 0)))
