(require-extension sys-plan9)

(define *debug* #t)
(define (debug msg) (if *debug* (format #t "~A~%" msg) #f))

(define *useragent* "useragent 's9fes webfs/0.1 (compatible; nope)'")
(define-structure webfs
	(path "") (ctl "") (payload '()) (cookies '()) (stop '()))

(define (create-webfs url)
	(let* ((id (car (read-file (open-input-file "/mnt/web/clone"))))
				 (path (string-append "/mnt/web/" id "/"))
				 (ctl (string-append path "ctl"))
				 (session
					(make-webfs	path ctl '()
						(sys:open ctl sys:OREAD) ; simply to keep the connection active
					)))
		(webfs:set-url session url)
		(debug (format #f "setting url: ~A" url))
		session
))

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

(define (webfs:set-url w url)
	(webfs:ctl-write w (string-append "url " url)))

(define (webfs:get w url)
	(let ()
		(webfs:set-url w url)
		(webfs:body w)))

(define (webfs:post w url body creds)
	(let ()
		(webfs:ctl-write w (string-append "url " url))
		(with-output-to-file (string-append (webfs-path w) "postbody")
			(lambda () (write body)))
		(webfs:body w)))

; with-input-from-file doesn't seem to work for this for some reason
(define (webfs:body w)
	(debug "reading...")
	(let* ((bpath (string-append (webfs-path w) "body"))
				 (bhandle (sys:open bpath sys:OREAD)))
			(let loop ((str "")
								 (b (sys:read bhandle 1024)))
				(if (eof-object? b) str
					(loop (string-append str b) (sys:read bhandle 1024))))))

;; ======= http code =======

(define (http:new-session url)
	(create-webfs url))

(define (http:set-url session url)
	(webfs:ctl-write session (string-append "url " url)))

(define (http:set-headers session headers) '())

(define (http:set-cookies session cookies) '())

(define (http:get-cookies session)
	(let* ((f (sys:open "/mnt/webcookies/http" sys:ORDWR))
				(url (read-file (open-input-file (string-append (webfs-path session) "parsed/url"))))
				(cookies ""))
	(debug (format #f "read url: ~A" (car url)))
	(sys:write f (car url))
	(set! cookies (sys:read f 4096))
	(debug (format #f "cookies: ~A" cookies))
	(sys:close f)
	cookies
))

(define (http:get url . options)
	(let ((w (create-webfs url)))
		(webfs:ctl-write w *useragent*)
		(webfs:get w url)))

(define (http:perform session)
	(if (null? (webfs-payload session))
		(webfs:body session)
		(webfs:post session)))

(define (http:post url body . creds)
	(let ((w (create-webfs url)))
		(webfs:ctl-write w *useragent*)
		(webfs:post w url body creds)))

(define (http:insecure session) '())
(define (http:enable_cookies) '())

; not working
(define (http:user:pass session user pass)
	(list (string-append "user " user)))

(define (http:basic_auth session) '())
