;;;; sync-quicklisp.asd

(asdf:defsystem #:sync-quicklisp
  :description "Pull down the quicklisp bits"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :depends-on (#:quicklisp-mirror-tool)
  :serial t
  :components ((:file "package")
               (:file "sync-quicklisp")))

