(load-from-library "9p.scm")
(load-from-library "hash-table.scm")

(define *9p:buffers* (make-hash-table))
(define *9p:qids* (make-hash-table))

(register-dir *9p:qids* 0 "" 0 sys:QTDIR)
(register-dir *9p:qids* 1 "ctl" #o0666 sys:QTFILE)

(define (fsread fc)
  (let* ((fid (fcall-fid fc))
         (offset (fcall-u1 fc))
         (msg (hash-table-ref *9p:buffers* fid)))
    (if msg
        (let* ((m (car msg))
               (length (string-length m)))
          (if (< offset length)
              (list fid (substring m offset length))
              (list (fcall-fid fc) "")))
        (begin (hash-table-set! *9p:buffers* fid "hello")
               (fsread fc)))))

(define (fsattach fc) (list (dir-qid (car (hash-table-ref *9p:qids* 0)))))

(define (fswalk fc)
	(let ((paths (vector->list (fcall-u2 fc))))
		(format #t "paths: ~A~%" paths)
		(if (null? paths) (list 0 (vector))
			(format #t "failure~%"))))

(define (fsstat fc)
	(let ((d (hash-table-ref *9p:qids* (fcall-u1 fc))))
		(if (not d) (list (vector (stat (car (hash-table-ref *9p:qids* 0)))))
			(list (vector (stat d))))))

(format #t "init~%")
(define fs (instance "fnord"))

(register fs "read" fsread)
(register fs "attach" fsattach)
(register fs "walk" fswalk)
(register fs "stat" fsstat)

(srv fs)
