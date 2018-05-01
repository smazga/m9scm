(require-extension sys-plan9)
(load-from-library "s9sos")

(define *useragent* "useragent 's9fes webfs/0.1 (compatible; nope)'")

(define-class webfs () path ctl stop)
(define-generic webfs:get)
(define-generic webfs:post)
(define-generic webfs:body)
(define-generic webfs:useragent)
(define-generic webfs:ctl-write)
(define-generic webfs:ctl-read)
(define-generic webfs:add-creds)

(define-method (initialize (w webfs))
	(let ((id (car (read-file (open-input-file "/mnt/web/clone")))))
		(slot-set! w 'path (string-append "/mnt/web/" id "/"))
		(slot-set! w 'ctl (string-append (slot-ref w 'path) "ctl"))
		; simply to keep the connection active
		(slot-set! w 'stop (sys:open (slot-ref w 'ctl) sys:OREAD))))

; not finished
(define-method (webfs:add-creds creds)
	(if (null? creds) #f #t))

(define-method (webfs:ctl-write (w webfs) msg)
	(let ((ctl (sys:open (slot-ref w 'ctl) sys:OWRITE)))
		(sys:write ctl msg)
		(sys:close ctl)))

(define-method (webfs:ctl-read (w webfs))
	(read-file (open-input-file (slot-ref w 'ctl))))

(define-method (webfs:get (w webfs) url)
	(let ()
		(webfs:ctl-write w (string-append "url " url))
		(webfs:body w)))

(define-method (webfs:post (w webfs) url body creds)
	(let ()
		(webfs:ctl-write w (string-append "url " url))
		(with-output-to-file (string-append (slot-ref w 'path) "postbody")
			(lambda () (write body)))
		(webfs:body w)))

; with-input-from-file doesn't seem to work for this for some reason
(define-method (webfs:body (w webfs))
	(let* ((bpath (string-append (slot-ref w 'path) "body"))
				 (bhandle (sys:open bpath sys:OREAD)))
			(let loop ((str "")
								 (b (sys:read bhandle 1024)))
				(if (eof-object? b) str
					(loop (string-append str b) (sys:read bhandle 1024))))))

; todo: It's likely too inefficient to create new connections every time.

(define (http-get url)
	(let ((w (make-instance webfs)))
		(webfs:ctl-write w *useragent*)
		(webfs:get w url)))

(define (http-post url body . creds)
	(let ((w (make-instance webfs)))
		(webfs:ctl-write w *useragent*)
		(webfs:post w url body creds)))
