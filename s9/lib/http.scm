;; S9 LIB  (http:get string)                       ==> string
;;         (http:post string string)               ==> string

;;         (http:new-session string)               ==> http session
;;         (http:basic-auth session)               ==> unspecific
;; 	(http:user:pass session string string)  ==> unspecific
;; 	(http:get-cookies session)              ==> vector
;; 	(http:get-cookie session string)        ==> string
;; 	(http:set-cookies session vector)       ==> unspecific

;;         (load-from-library "http:scm")

;; The goal is to make the HTTP library cross-platform. At the moment importing
;; HTTP.SCM loads the curl extension in unix and the webfs in plan9.

;; HTTP:NEW-SESSION creates and returns a session object appropriate for the
;; platform: a curl-easy instance for unix, and a webfs instance for plan9.

;; HTTP:GET takes a url and returns the response as a string.

;; HTTP:POST takes a url and a payload and returns the response as a string.

;; HTTP:BASIC-AUTH enables http basic auth behavior on the session.

;; HTTP:USER:PASS sets the basic auth credentials on the session.

;; HTTP:GET-COOKIES returns all known cookies from give session.

;; HTTP:GET-COOKIE searches for a cookie that matches the passed REGEX and
;; returns it.

;; HTTP:SET-COOKIES sets cookies on the session according to the vector passed.

(let ((platform (symbol->string *host-system*)))
  (cond ((string=? "plan9" platform)
	 (load-from-library "webfs.scm"))
	((string=? "unix" platform)
	 (load-from-library "curl.scm"))))
