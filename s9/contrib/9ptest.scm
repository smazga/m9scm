(load-from-library "9p.scm")
(load-from-library "hash-table.scm")
(load-from-library "id.scm")
(load-from-library "type-case.scm")

(define *9p:buffers* (make-hash-table))
(define *9p:qids* (make-hash-table))

(register-dir *9p:qids* 0 "foo" #o0666 sys:QTDIR)
(register-dir *9p:qids* 1 "ctl" #o0666 sys:QTFILE)

;; NOTES:
;;   * currently getting back bogus info for read()...I think it wants a Dir or something
;;   * /sys/src/cmd/aux/searchfs.c is a good reference

;;(define (fsread fc)
;;  (let* ((fid (fcall-fid fc))
;;         (offset (fcall-u1 fc))
;;         (msg (hash-table-ref *9p:buffers* fid)))
;;    (if msg
;;        (let* ((m (car msg))
;;               (length (string-length m)))
;;          (if (< offset length)
;;              (list fid (substring m offset length))
;;              (list (fcall-fid fc) "")))
;;        (begin (hash-table-set! *9p:buffers* fid "hello")
;;               (fsread fc)))))

(define (fsread fc)
	(let* ((fid (fcall-fid fc))
				 (offset (fcall-u1 fc))
				 (qid (stat (car (hash-table-ref *9p:qids* 0)))))
				 (format #t "type of qid: ~A~%" (type-of qid))
				 (format #t "qid: ~A~%" qid)
				 (format #t "offset: ~A~%" offset)
				 (format #t "fsread fid: ~A~%" fid)
				 (if (> offset 0) '(0 "")
				 	(list fid qid))))

(define (fsattach fc) (list (dir-qid (car (hash-table-ref *9p:qids* 0)))))

(define (fswalk fc)
	(let ((paths (vector->list (fcall-u2 fc)))
				(root (car (hash-table-ref *9p:qids* 0))))
		(format #t "paths: ~A~%" paths)
		(if (null? paths) (list (vector))
			(format #t "failure~%"))))

(define (fsstat fc)
	(let ((d (hash-table-ref *9p:qids* (fcall-u1 fc))))
		(if (not d) (list (vector (stat (car (hash-table-ref *9p:qids* 0)))))
			(list (vector (stat d))))))

(define (fsclunk fc)
	(let* ((fid (fcall-fid fc))
		 (entry (hash-table-ref *9p:buffers* fid)))
		(if (not (false entry))
			(hash-table-remove! *9p:buffers* fid))
		(list fid)))
		
(define (fsopen fc)
	(let* ((fid (fcall-fid fc))
				 (entry (fcall-u1 fc))
				 (qid (hash-table-ref *9p:qids* entry)))
		(if (false qid) (list fid)
			(list qid))))

(format #t "init~%")
(define fs (instance "fnord"))

(register fs "read" fsread)
(register fs "attach" fsattach)
(register fs "walk" fswalk)
(register fs "stat" fsstat)
(register fs "clunk" fsclunk)
(register fs "open" fsopen)

(srv fs)
