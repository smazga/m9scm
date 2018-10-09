(cond ((eqv? "plan9" (platform))
       (load-from-library "webfs.scm"))
      ((eqv? "unix" (platform))
       (load-from-library "curl.scm")))
