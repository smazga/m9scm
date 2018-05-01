(require-extension sys-plan9)
(load-from-library "simple-modules.scm")
(load-from-library "hash-table.scm")

(module 9p
	; attrs: attach auth clone create flush open read remove stat write wstat walk walk1
	(define-structure fs srvname funcs)
	(define-structure fcall type tag fid (u1 '()) (u2 '()) (u3 '()))
	(define msize 8192)
	(define version "9P2000")

	(define (handle f fc)
		(let* ((ftype (symbol->string (fcall-type fc))
					 (handler (hash-table-ref (fs-funcs) ftype)))
					 (response (string-append "R" (substring ftype 1 (string-length ftype)))))
			(if (not handler) (vector "Rerror")
				(vector (append (list response (fcall-tag fc)) (handler))))))

	(define* (srv f)
		(let ((pipe (initsrv fs-srvname f))))
			(let loop ((msg (sys:read9pmsg pipe)))
				(if (zero? (string-length msg)) (format #t "disconnected")
					(let* ((fc (apply make-fcall (vector->list (sys:convM2S msg))))
								 (result (handle f fc)))
						(format #t "read: ~A~%" fc)
						(format #t "result: ~A~%" result)
						(sys:write pipe (sys:convS2M result))
						(loop (sys:read9pmsg pipe))))))
	
	(define (initsrv srvname)
		(let* ((p (sys:pipe))
				(ptmp (car p))
				(pipe (cadr p))
				(srvpath (string-append "/srv/" srvname))
				(srvfd (sys:create srvpath sys:OWRITE #o0600)))
			(format #t "serving ~A on fd ~D~%" srvpath pipe)
			(defer (sys:remove srvpath))
			(sys:write srvfd (format #f "~D" ptmp))
			(sys:close srvfd)
			(sys:close ptmp)
			pipe))

	(define* (instance srvname)
		(let ((f (make-fs srvname))
					(h (make-hash-table)))
			(hash-table-set! h "Tversion" (list msize version))
			(format #t "f: ~A~%" f)
			(format #t "h: ~A~%" h)
			(fs-set-funcs! f h)))


(define-syntax 9p:fs
	(lambda (srvname)
		`(format #t "9p:fs: ~A~%" ,srvname)
		`(using 9p (instance) (instance ,srvname))))

(define-syntax 9p:srv
	(lambda (fs)
		`(format #t "9p:srv: ~A~%" ,fs)
		`(using 9p (srv) (srv ,fs))))
