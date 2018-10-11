(define (http:get url . options)
  (let ((easy (curl:easy-init)))
    (curl:setopt easy "CURLOPT_URL" url)
    (for-each (lambda (i)
		(if (list? i)
		 (for-each (lambda (j)
			     (curl:setopt easy (car j) (cdr j))) i)
		 (curl:setopt easy (car i) (cdr i))))
	      options)
    (curl:perform easy)
    (curl:cleanup easy)))

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
