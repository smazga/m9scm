(load-from-library "s9sos.scm")

(define-class 9p:entry ())
(define-class :9p:dir (9p:entry))
(define-class :9p:file (9p:entry))
(define-generic 9p:read)

(define-method (9p:read (f 9p:file) fc)
	(let ((offset (fcall-u1 fc))
				(contents (slot-ref f 'contents)))
		(substring contents offset (string-length contents))))
		
(define-method (9p:read (d 9p:dir) fc)
	(let ((offset (fcall-u1 fc))
				(contents "bogusdirectory"))
		(substring contents offset (string-length contents))))

