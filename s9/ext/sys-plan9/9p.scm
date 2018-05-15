(require-extension sys-plan9)
(load-from-library "simple-modules.scm")
(load-from-library "hash-table.scm")

;(module 9p
	; actions: attach auth clone create flush open read remove stat version write wstat walk
	(define-structure fs srvname funcs)
	(define-structure fcall action tag fid (u1 '()) (u2 '()) (u3 '()))
	(define-structure dir
		(type 0) (dev 0) (qid "") (mode 0) (atime 0) (mtime 0)
		(length 0) (name "") (uid "nobody") (gid "nobody") (muid "nobody"))

	(define msize 8192)
	(define version "9P2000")
	
	(define (stat d)
		(vector (string->symbol "dir")
						(dir-type d)
						(dir-dev d)
						(dir-qid d)
						(dir-mode d)
						(dir-atime d)
						(dir-mtime d)
						(dir-length d)
						(dir-name d)
						(dir-uid d)
						(dir-gid d)
						(dir-muid d)))

	(define (handle f fc)
		(let* ((x (format #t "handle::~A~%" fc))
					 (ftype (symbol->string (fcall-action fc)))
					 (action (substring ftype 1 (string-length ftype)))
					 (handler (hash-table-ref (fs-funcs f) action))
					 (response (string->symbol (string-append "R" action))))
			(format #t "read: ~A~%" fc)
			(format #t "action: ~A~%" action)
			(if (not (list? handler)) (vector (string->symbol "Rerror") (fcall-tag fc)
																	(format #f "unhandled function: ~A" action))
				(list->vector (append (list response (fcall-tag fc)) ((car handler) fc))))))

	(define (register f action func)
		(hash-table-set! (fs-funcs f) action func))

	(define (srv f)
		(let ((pipe (initsrv (fs-srvname f))))
			(let loop ((msg (sys:read9pmsg pipe)))
				(if (zero? (string-length msg)) (format #t "disconnected")
					(let* ((fc (apply make-fcall (vector->list (sys:convM2S msg))))
								 (result (handle f fc)))
						(format #t "result: ~A~%" result)
						(if (vector? result) (sys:write pipe (sys:convS2M result)))
						(loop (sys:read9pmsg pipe)))))))
	
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

	(define (instance srvname)
		(let ((f (make-fs srvname (make-hash-table))))
			(register f "version" versionstub)
			f))

	(define (versionstub fc) (list msize version))
	(define (authstub fc) (list '() '()))


(define-syntax 9p:fs
	(lambda (srvname)
		`(format #t "9p:fs: ~A~%" ,srvname)
		`(instance ,srvname)))
;		`(using 9p (instance) (instance ,srvname))))

(define-syntax 9p:srv
	(lambda (f)
		(format #t "9p:srv: ~A~%" f)
		`(srv ,f)))
;		`(using 9p (srv) (srv ,f))))
