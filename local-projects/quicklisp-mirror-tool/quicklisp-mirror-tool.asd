#|
  This file is a part of quicklisp-mirror-tool project.
  Copyright (c) 2016 David Gu (david_guru@gty.org.in)
|#

#|
  Quicklisp-Mirror-Tool, helping people building mirror sites for Quicklisp.

  Author: David Gu (david_guru@gty.org.in)
|#

(in-package :cl-user)
(defpackage quicklisp-mirror-tool-asd
  (:use :cl :asdf))
(in-package :quicklisp-mirror-tool-asd)

(defsystem quicklisp-mirror-tool
  :version "0.1"
  :author "David Gu"
  :license "MIT"
  :depends-on (#-asdf3 :uiop
               :split-sequence
               :ironclad
               :bordeaux-threads
               :lparallel
               :quri
               :drakma
               :trivial-download
               :cl-reexport)
  :components ((:file "variables")
               (:file "utils")
               (:file "task")
               (:file "client")
               (:file "dist")
               (:file "quicklisp-mirror-tool"))
  :description "Quicklisp-Mirror-Tool, helping people building mirror sites for Quicklisp."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))
