;;;; variables.lisp
(in-package #:cl-user)
(defpackage #:quicklisp-mirror-tool.variables
  (:use #:cl)
  (:nicknames #:ql-mirror-tool.variables
              #:ql-mirror.variables)
  (:export #:*quicklisp-mirror-hostname*
           #:*web-server-directory-root*
           #:*available-client-versions*
           #:*available-dist-versions*
           #:*number-of-workers*
           #:*tasks-record-lock*
           #:*processing-style*))
(in-package #:quicklisp-mirror-tool.variables)

;;; hostname
(defvar *quicklisp-mirror-hostname* "localhost:80"
  "Definition for your mirror's hostname, e.g. \"beta.quicklisp.org\"(which
is also the original hostname of Quicklisp).")

;;; www directory
;;; tips: you may wanna make this directory owned by the current user
(defvar *web-server-directory-root* "/opt/app-root/quicklisp/local-projects/quicklisp-mirror"
  "It defines where you put all mirrored file at, e.g. \"/usr/local/var/www\", which
is the default Docroot of Nginx on Mac OS X (fetched from Homebrew).")

;;; number of workers, add it if you want --
;;; but just specify a number that fits your machine's ability
(defvar *number-of-workers* 8
  "The number of workers that will be working on mirroring various tars and text files.")

;;; this is one of the reasons why you still need a Quicklisp on server side ...
(defvar *available-client-versions* (ql:available-client-versions)
  "All available Quicklisp client versions.")
(defvar *available-dist-versions* (ql:available-dist-versions "quicklisp")
  "All available \"official\" Quicklisp dist versions.")

(defvar *tasks-record-lock* (bt:make-recursive-lock "TASKS-RECORD")
  "The MUTEX lock which will be used in 'do-tasks-record'.")

(defvar *processing-style* :seq
  "The style for processing multiple tasks, either :seq (means sequentially) or :parallel")
