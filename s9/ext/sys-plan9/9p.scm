(require-extension sys-plan9)
(load-from-library "simple-modules.scm")
(load-from-library "hash-table.scm")
(load-from-library "appendb.scm")

(define *9p:paths-table* (make-hash-table))
(define *9p:path-map* (make-hash-table))
(define *9p:path-id* 0)
(define *9p:tree* '())
(define *9p:root* '())

;(module 9p
  ;; actions: attach auth clone create flush open read remove stat version write wstat walk
  (define-structure fs srvname funcs)
  (define-structure fcall action tag fid (u1 '()) (u2 '()) (u3 '()))
  (define-structure dir
    (type 0) (dev 0) (qid "") (mode 0) (atime 0) (mtime 0) (length 0)
    (name "") (uid "nobody") (gid "nobody") (muid "nobody") (path 0)
    (contents '()) (msg ""))
    ;; TODO: it's silly to store the converted message here and convert it in
    ;; sys_convS2M, but we do it to be able to understand the offsets

	(define (next-path-id)
		(let ((id *9p:path-id*))
			(set! *9p:path-id* (+ *9p:path-id* 1))
			id))
	
	(define (parse-path tree)
		(let* ((dir (car tree))
					 (path (dir-path dir))
					 (contents (dir-contents dir)))
			(format #t "ADDING ~A~%" dir)
			(format #t "CONTENTS (~A): ~A~%" (type-of contents) contents)
			(hash-table-set! *9p:paths-table* path dir)
			(cond ((null? contents) #t
						 (pair? contents) (parse-path contents)
						 (list? contents)
							(for-each (lambda (x)
								(parse-path x) contents))
						 (default (parse-path contents))))))

	(define (9p:root mode . contents)
		(let ((root (9p:entry "" mode sys:QTDIR contents)))
			(set! *9p:root* root)))

	(define (9p:dir name mode . contents)
		(9p:entry name mode sys:QTDIR contents))

	(define (9p:file name mode . contents)
		(9p:entry name mode sys:QTFILE contents))

  (define (9p:entry name mode type contents)
    (let ((d (make-dir))
    			(path (next-path-id)))
      (dir-set-qid! d (format #f "~X:~X:~X" path mode type))
      (dir-set-name! d name)
    	(dir-set-path! d path)
      (dir-set-contents! d contents)
      (dir-set-msg! d (sys:convD2M (stat d)))
      (hash-table-set! *9p:path-map* path d)
      d))

  (define (get-entry path)
  	(car (hash-table-ref *9p:path-map* path)))

  (define (get-root)
  		(car (hash-table-ref *9p:path-map* (dir-path *9p:root*))))

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
      (format #t "fid: ~A~%" (fcall-fid fc))
      (if (not (list? handler))
	  (vector (string->symbol "Rerror") (fcall-tag fc)
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
    	(if (null? *9p:tree*) (error "*9p:tree* must be defined"))
;;    		(parse-path *9p:tree*))
    	(format #t "PATHS: ~A~%" *9p:path-map*)
      (register f "version" versionstub)
      f))

  (define (versionstub fc) (list msize version))
  (define (authstub fc) (list '() '()))
;)

(define-syntax 9p:fs
  (lambda (srvname)
    `(format #t "9p:fs: ~A~%" ,srvname)
    ;; `(instance ,srvname)))
    `(using 9p (instance) (instance ,srvname))))

(define-syntax 9p:srv
  (lambda (f)
    (format #t "9p:srv: ~A~%" f)
    ;; `(srv ,f)))
    `(using 9p (srv) (srv ,f))))
