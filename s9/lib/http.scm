;; Try to implement the following in both implementations so that http stuff can be platform agnostic

;; http:basic_auth		enable http basic auth
;; http:user:pass               take a user/pass combo and create a basic auth digest
;; http:insecure                turn off ssl safety checks

(cond ((string=? "plan9" (platform))
       (load-from-library "webfs.scm"))
      ((string=? "unix" (platform))
       (load-from-library "curl.scm")))
