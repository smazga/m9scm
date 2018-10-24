;; Try to implement the following in both implementations so that http stuff can be platform agnostic

;; (http:get url) => string
;;	execute a GET and return the response
;; (http:post url postdata) => string
;;	execute a POST and return the response

;; (http:basic_auth session) => unspecific
;;	enable http basic auth
;; (http:user:pass session user pass) =>unspecific
;;	take a user/pass combo and create a basic auth digest
;; (http:insecure session) => unspecific
;;	turn off ssl safety checks
;; (http:get-cookies session) => vector
;;	return cookies
;; (http:get-cookie session regex) => string
;;	return a single cookie matched with a regex
;; (http:set-cookies session cookies) => unspecific
;;	set the cookies to the passed vector

(cond ((string=? "plan9" (platform))
       (load-from-library "webfs.scm"))
      ((string=? "unix" (platform))
       (load-from-library "curl.scm")))
