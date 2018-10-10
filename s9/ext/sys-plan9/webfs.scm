(require-extension sys-plan9)

(define *useragent* "useragent 's9fes webfs/0.1 (compatible; nope)'")
(define-structure webfs (path "") (ctl "") (stop '()))

(define (create-webfs url)
	(let* ((id (car (read-file (open-input-file "/mnt/web/clone"))))
				 (path (string-append "/mnt/web/" id "/"))
				 (ctl (string-append path "ctl")))
		(make-webfs
			path
			ctl
			(sys:open ctl sys:OREAD) ; simply to keep the connection active
)))

; not finished
(define (webfs:add-creds creds)
	(if (null? creds) #f #t))

(define (webfs:ctl-write w msg)
	(let ((ctl (sys:open (webfs-ctl w) sys:OWRITE)))
		(sys:write ctl msg)
		(sys:close ctl)))

(define (webfs:ctl-read w)
	(read-file (open-input-file (webfs-ctl w))))

(define (webfs:path-write w msg)
	(let ((path (sys:open (webfs-path w) sys:OWRITE)))
		(sys:write path msg)
		(sys:close path)))

(define (webfs:path-read w dir)
	(read-file (open-input-file (string-append (webfs-path w) dir))))

(define (webfs:get w url)
	(let ()
		(webfs:ctl-write w (string-append "url " url))
		(webfs:body w)))

(define (webfs:post w url body creds)
	(let ()
		(webfs:ctl-write w (string-append "url " url))
		(with-output-to-file (string-append (webfs-path w) "postbody")
			(lambda () (write body)))
		(webfs:body w)))

; with-input-from-file doesn't seem to work for this for some reason
(define (webfs:body w)
	(let* ((bpath (string-append (webfs-path w) "body"))
				 (bhandle (sys:open bpath sys:OREAD)))
			(let loop ((str "")
								 (b (sys:read bhandle 1024)))
				(if (eof-object? b) str
					(loop (string-append str b) (sys:read bhandle 1024))))))

; todo: It's likely too inefficient to create new connections every time.

(define (http:get url . options)
	(let ((w (create-webfs url)))
		(webfs:ctl-write w *useragent*)
		(webfs:get w url)))

(define (http:post url body . creds)
	(let ((w (create-webfs url)))
		(webfs:ctl-write w *useragent*)
		(webfs:post w url body creds)))

(define (http:insecure) '())
(define (http:enable_cookies) '())

; not working
(define (http:user:pass user pass)
	(list (string-append "user " user)))

(define (http:basic_auth) '())
