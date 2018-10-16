(define (http:get url . options)
  (let ((easy (curl:easy-init)))
    (curl:setopt easy "CURLOPT_URL" url)
    (apply-options easy options)
    (let ((response (curl:perform easy)))
      (curl:cleanup easy)
      response)))

(define (apply-options easy options . rest)
  (cond ((null? options) #t)
	((list? options) (apply-options easy (car options) (cdr options)))
	(else
	 (begin
	   (curl:setopt easy (car options) (cdr options))
	   (apply-options easy rest)))))

(define (http:enable_cookies file)
  '("CURLOPT_COOKIEFILE" . file))

(define (http:basic_auth)
  (cons "CURLOPT_HTTPAUTH" (sys:magic-const "CURLAUTH_BASIC")))

(define (http:user:pass user pass)
  (cons "CURLOPT_USERPWD" (format #f "~A:~A" user pass)))

(define (http:insecure)
  (list
   '("CURLOPT_SSL_VERIFYHOST" . 0)
   '("CURLOPT_SSL_VERIFYPEER" . 0)))
