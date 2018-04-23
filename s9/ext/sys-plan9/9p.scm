(require-extension sys-plan9)
(load-from-library "s9sos")

(define-class 9p:fs ()
	attach
	auth
	clone
	create
	flush
	open
	read
	remove
	stat
	write
	wstat
	walk
	walk1)
(define-generic 9p:srv)

(define-method (9p:srv (f 9p:fs) srvname)
	(let ((pipe (9p:initsrv srvname)))
		(let loop ((msg (sys:read9pmsg pipe)))
			(if (zero? (string-length msg)) (format #t "disconnected")
				(begin
					(format #t "read: ~A~%" msg)
					(loop (sys:read9pmsg pipe)))))))

(define (9p:initsrv srvname)
	(let* ((p (sys:pipe))
			(ptmp (car p))
			(pipe (cadr p))
			(srvfd (sys:create (string-append "/srv/" srvname) sys:OWRITE #o0600)))
		(format #t "serving /srv/~A on fd ~D~%" srvname pipe)
		(sys:write srvfd (format #f "~D" ptmp))
		(sys:close srvfd)
		(sys:close ptmp)
		pipe))

