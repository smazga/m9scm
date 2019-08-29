(load-from-library "9p.scm")
;;(load-from-library "hash-table.scm")
(load-from-library "id.scm")
(load-from-library "type-case.scm")

(define *9p:buffers* (make-hash-table))
(define *9p:qids* (make-hash-table))

(define *tree*
	(9p:root #o0666
		(9p:dir "foo" #o0666
			(9p:file "bar" #o0666 "file contents")
			(9p:file "bar2" #o0666 "file2 contents"))
		(9p:file "baz" #o0666 "more file contents")))

;; NOTES:
;;   * /sys/src/cmd/aux/searchfs.c is a good reference

(define (fsread fc)
	(let* ((fid (fcall-fid fc))
				 (offset (fcall-u1 fc))
				 (qid (get-root)))
				 (format #t "qid: ~A~%" qid)
				 (format #t "offset: ~A~%" offset)
				 ;;(format #t "fsread fid: ~A~%" fid)
				 (if (>= offset (string-length (dir-msg qid))) '(0 "")
				 	(list fid (stat qid)))))

(define (fsattach fc) (list (dir-qid (get-root))))

(define (fswalk fc)
	(let ((paths (vector->list (fcall-u2 fc)))
				(root (get-entry 0)))
		(format #t "paths: ~A~%" paths)
		(if (null? paths) (list (vector))
			(format #t "failure~%"))))

(define (fsstat fc)
	(let ((d (hash-table-ref *9p:qids* (fcall-u1 fc))))
		(if (not d) (list (vector (stat (get-root))))
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

;;(define (dump-tree path rest)
;;  (let ((dir (car path))
;;        (contents (cdr path)))
;;    (format #t "dir: ~A~%" dir)))

(define (dump-tree path)
  (let ((dir (dir-name path))
		(contents (dir-contents path)))
    (format #t "dir: ~A~%" dir)
	(dump-tree contents)))

(format #t "init~%")
(format #t "TREE: ~A~%" (dir-contents *tree*))
(format #t "  type: ~A~%" (type-of *tree*))
(format #t "  root: ~A~%" (dir-path (get-root)))
;; (dump-tree (get-entry 0))
(define fs (instance "fnord" *tree*))

(register fs "read" fsread)
(register fs "attach" fsattach)
(register fs "walk" fswalk)
(register fs "stat" fsstat)
(register fs "clunk" fsclunk)
(register fs "open" fsopen)

(format #t "ROOT: ~A~%" (get-root))

(srv fs)
