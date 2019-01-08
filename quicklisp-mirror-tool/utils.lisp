;;;; utils.lisp
(in-package #:cl-user)
(defpackage #:quicklisp-mirror-tool.utils
  (:use #:cl #:split-sequence
        #:ql-mirror.variables)
  (:nicknames #:ql-mirror.utils)
  (:export #:modify-hostname
           #:modify-archive-url
           #:verify-file-md5
           #:get-filename-from-url))
(in-package #:quicklisp-mirror-tool.utils)

(declaim (inline modify-hostname))
(defun modify-hostname (url)
  "Modify the hostname part for an url,
e.g. \"http://beta.quicklisp.org/client/2014-01-28/client-info.sexp\" =>
\"http://quicklisp-mirror.cn/client/2014-01-28/client-info.sexp\""
  (declare (type simple-string url))
  (let ((uri (quri:uri url)))
    (setf (quri:uri-host uri) *quicklisp-mirror-hostname*)
    (quri:render-uri uri)))

(defun modify-archive-url (url &optional (prefix "/quicklisp"))
  "Given a project archive's url, modify its hostname first, then modify its path,
e.g. \"http://beta.quicklisp.org/archive/1am/2014-11-06/1am-20141106-git.tgz\" => \"http://quicklisp-mirror.cn/archive/quicklisp/1am/2014-11-06/1am-20141106-git.tgz\""
  (declare (type simple-string url))
  (let ((uri (quri:uri url)))
    (setf (quri:uri-host uri) *quicklisp-mirror-hostname*)
    (let* ((path (quri:uri-path uri))
           (next-slash-position (position #\/ path :start 1 :test 'char=)))
      (setf (quri:uri-path uri)
            (concatenate 'string
                         (subseq path 0 next-slash-position)
                         prefix
                         (subseq path next-slash-position)))
      (values (quri:render-uri uri) (quri:uri-path uri)))))

(declaim (inline verify-file-md5))
(defun verify-file-md5 (pathspec md5)
  "Verify a file by a given md5 string."
  (declare (type simple-string md5))
  (if (string= (ironclad:byte-array-to-hex-string
                (ironclad:digest-file :md5 pathspec))
               md5)
      t
      (error "Verfication of ~S failed." pathspec)))

(declaim (inline get-filename-from-url))
(defun get-filename-from-url (url)
  (declare (type simple-string url))
  (let* ((path (quri:uri-path (quri:uri url)))
         (name (pathname-name path))
         (type (pathname-type path)))
    (concatenate 'string name "." type)))
