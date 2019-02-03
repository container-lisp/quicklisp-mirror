;;;; client.lisp
(in-package #:cl-user)
(defpackage #:quicklisp-mirror-tool.client
  (:use #:cl
        #:ql-mirror.variables
        #:ql-mirror.utils
        #:ql-mirror.task)
  (:nicknames #:ql-mirror.client)
  (:export #:*client-directory*
           #:client-tasks/fetching*
           #:client-tasks/retry-fetching*
           #:client-tasks/verifying*
           #:client-tasks/retry-verifying*
           #:fetch-quicklisp.sexp
           #:fetch-quicklisp-versions.sexp
           #:initialize/client
           #:mirror/client
           #:verify/client))
(in-package #:quicklisp-mirror-tool.client)

;;; Global Variables
(defvar *client-directory*
  (let ((directory* (uiop:directory-exists-p *web-server-directory-root*)))
    (unless directory*
      (error "Directory: ~A doesn't exist." *web-server-directory-root*))
    (merge-pathnames "client/" (uiop:ensure-absolute-pathname directory*)))
  "The directory where all client information files would be put at.")

(defvar *client-tasks/fetching*
  (make-tasks-record :name "client-tasks/fetching"
                     :capacity (+ 2 ;; fetch-quicklisp.sexp + fetch-quicklisp-versions.sexp
                                  ;; quicklisp.tar + setup.lisp + asdf.lisp
                                  (* 3 (length *available-client-versions*))))
  "Tasks-Record for fetching client files, including 'client-info.sexp', 'quicklisp.tar', 'setup.lisp' and 'asdf.lisp' for each version")

(defvar *client-tasks/retry-fetching*
  (make-tasks-record :name "client-tasks/retry-fetching")
  "Tasks-Record for retrying fetching broken (during downloading) files .")

(defvar *client-tasks/verifying*
  (make-tasks-record :name "client-tasks/verification"
                     :capacity (* 3 (length *available-client-versions*)))
  "Tasks-Record for verifying 'quicklisp.tar', 'setup.lisp' and 'asdf.lisp' for each client version.")

(defvar *client-tasks/retry-verifying*
  (make-tasks-record :name "client-tasks/retry-verifying")
  "Tasks-Record for retrying verifying broken (checked by md5) files.")

;;; Utilities
(declaim (inline make-dir-by-client-version))
(defun make-dir-by-client-version (version)
  "Given a version, e.g. \"2016-02-22\",
then transform it to #P\"/usr/local/var/www/client/2016-02-22/\" and return."
  (declare (type simple-string version))
  (merge-pathnames (concatenate 'string version "/")
                   *client-directory*))

#+ignore
(defmacro with-client-info-slots ((&rest slots) client-info &body body)
  "An utility macro helps with binding slots of a client info using 'symbol-macrolet'."
  (alexandria:with-gensyms (%getf% indicator indicators result)
    `(flet ((,%getf% (,indicators)
              (loop for ,indicator in ,indicators
                 for ,result = (getf ,client-info ,indicator) then (getf ,result ,indicator)
                 finally (return ,result))))
       (symbol-macrolet ,(mapcar (lambda (slot)
                                   (cond ((atom slot)
                                          `(,slot (getf ,client-info ,(intern (symbol-name slot) :keyword))))
                                         ((consp slot)
                                          `(,(car slot) (,%getf% ',(cadr slot))))))
                                 slots)
         ,@body))))

(defun modify-client-info-hostname (client-info)
  "Modify hostnames for all urls in a client infomation file (e.g. 'client-info.sexp')."
  (declare (type cons client-info))
  (destructuring-bind (&key subscription-url canonical-client-info-url
                            client-tar setup asdf
                            &allow-other-keys)
      client-info
    (setf (getf client-info :subscription-url) (modify-hostname subscription-url)
          (getf client-info :canonical-client-info-url) (modify-hostname canonical-client-info-url)
          (getf (getf client-info :client-tar) :url) (modify-hostname (getf client-tar :url))
          (getf (getf client-info :setup) :url) (modify-hostname (getf setup :url))
          (getf (getf client-info :asdf) :url) (modify-hostname (getf asdf :url)))
    (let ((*print-case* :downcase))
      (write-to-string client-info))))

(defun collect-required-urls (client-info)
  "Collect client-tar, setup and asdf's urls from a 'client-info.sexp'."
  (declare (type list client-info))
  (destructuring-bind (&key client-tar setup asdf &allow-other-keys)
      client-info
    (mapcar (lambda (x) (getf x :url))
            (list client-tar setup asdf))))

(defun download-tmp-client-info.sexp (version url &key keep)
  "Before fetching files, we need a temporary copy of the original 'client-info.sexp' as metadata.
After all tasks are prepared, we will then delete these files.
If 'keep' was given as T, the downloaded file will be kept; delete the file when it's NIL."
  (let* ((tmp-dir (uiop:ensure-directory-pathname (format nil "/tmp/~A/" version)))
         (tmp-filename (merge-pathnames "client-info.sexp" tmp-dir)))
      (if (probe-file tmp-filename)
          tmp-filename
          (progn
            (trivial-download:download url tmp-filename :quiet nil)
            (terpri *standard-output*)
            (unless keep
              (uiop:delete-directory-tree tmp-dir :validate t :if-does-not-exist :ignore))
            tmp-filename))))           

(declaim (inline delete-tmp-downloaded-client-info))
(defun delete-tmp-downloaded-client-info (file)
  "Delete the temporary original 'client-info.sexp' file."
  (uiop:delete-directory-tree
   (uiop:pathname-directory-pathname file)
   :validate t :if-does-not-exist :ignore))

;;; Functions to prepare all tasks
;;; -- Utilities -----------------
(defun fetch-quicklisp.sexp
    (&optional (url "http://beta.quicklisp.org/client/quicklisp.sexp"))
  "A default client info file should be fetched; its link was given as 'subscription-url'."
  (let ((quicklisp.sexp (merge-pathnames "quicklisp.sexp" *client-directory*))
        (client-info (read-from-string (drakma:http-request url))))
    (with-open-file (out quicklisp.sexp :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
      (write-sequence
       (modify-client-info-hostname client-info) out))
    (probe-file quicklisp.sexp)))

(defun fetch-quicklisp-versions.sexp
    (&optional (url "http://beta.quicklisp.org/client/quicklisp-versions.sexp"))
  "A defaut client versions file should be fetched;
its link was defined by appending \"-versions\" to the name of subscription url."
  (let ((quicklisp-versions.sexp (merge-pathnames "quicklisp-versions.sexp" *client-directory*))
        (sexp (read-from-string (drakma:http-request url))))
    (with-open-file (out quicklisp-versions.sexp :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
      (write-sequence
       (write-to-string
        (dolist (x sexp sexp)
          (setf (cdr x) (modify-hostname (cdr x)))))
       out))
    (probe-file quicklisp-versions.sexp)))

(declaim (inline %prepare-fetching-quicklisp.sexp% %prepare-fetching-quicklisp-veriosn.sexp%))
(defun %prepare-fetching-quicklisp.sexp% ()
  "Submit 'fetching quicklisp.sexp' as a task/fetching to *client-tasks/fetching*."
  (submit-task
   *client-tasks/fetching*
   (make-task/fetching "http://beta.quicklisp.org/client/quicklisp.sexp"
                       (merge-pathnames "quicklisp.sexp" *client-directory*)
                       :name "FETCHIING QUICKLISP.SEXP [CLIENT]"
                       :thunk #'fetch-quicklisp.sexp)))
(defun %prepare-fetching-quicklisp-veriosn.sexp% ()
  "Submit 'fetching quicklisp-versions.sexp' as a task/fetching to *client-tasks/fetching*."
  (submit-task
   *client-tasks/fetching*
   (make-task/fetching "http://beta.quicklisp.org/client/quicklisp-versions.sexp"
                       (merge-pathnames "quicklisp-versions.sexp" *client-directory*)
                       :name "FETCHING QUICKLISP-VERSIONS.SEXP [CLIENT]"               
                       :thunk #'fetch-quicklisp-versions.sexp)))

(defun prepare-fetching (version url &key client-info)
  "Given the metadata (version and url of a client), prepare all fetching tasks.
If 'client-info' was given, which should be an original copy, then read required infomation from it;
otherwise, download a temporary copy."
  (let ((output-dir (make-dir-by-client-version version)))
    (ensure-directories-exist output-dir :verbose t)
    (let* ((client-info
            (if client-info client-info
                (read-from-string (uiop:read-file-string (download-tmp-client-info.sexp version url)))))
           (required-urls (collect-required-urls client-info)))
      (declare (type list client-info))
      (dolist (url required-urls)
        (let* ((filename (get-filename-from-url url))
               (destination (merge-pathnames filename output-dir)))
          (submit-task
           *client-tasks/fetching*
           (make-task/fetching url destination
                               :name (format nil "FETCHING ~S for CLIENT[~A]" filename version))))))))

(defun prepare-verifying (version &key client-info)
  "Given the metadata (version and url of a client), prepare all verifying tasks.
If 'client-info' was given, which should be an original copy, then read required infomation from it;
otherwise, download a temporary copy and then read from it."
  (let* ((dir (uiop:ensure-directory-pathname (merge-pathnames version *client-directory*)))
         (client-info
          (if client-info client-info
              (read-from-string (uiop:read-file-string (merge-pathnames "client-info.sexp" dir)))))
         (files (mapcar (lambda (x) (merge-pathnames x dir))
                        '("quicklisp.tar" "setup.lisp" "asdf.lisp"))) 
         (md5s (mapcar (lambda (x) (getf (getf client-info x) :md5))
                       '(:client-tar :setup :asdf)))
         (urls (mapcar (lambda (x) (getf (getf client-info x) :url))
                       '(:client-tar :setup :asdf))))
    (map nil (lambda (file md5 url)
               (submit-task *client-tasks/verifying*
                            (make-task/verifying :file file :md5 md5 :url url)))
         files md5s urls)))

(defun write-new-client-info (version original-client-info)
  "After all tasks were prepared and submited, a new client-info where all urls' hostnames have been modified should be written."
  (let ((new-client-info
         (merge-pathnames "client-info.sexp" (make-dir-by-client-version version))))
    (with-open-file (out new-client-info :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
      (write-sequence (modify-client-info-hostname original-client-info) out))))

;;; -- 1st -- Initialization. ------------------------------
(defun initialize/client (&optional (metadata *available-client-versions*))
  "Initialize all original client-info.sexp metadata and all fetching and verifying tasks."
  (declare (optimize speed (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (ensure-tasks-record-empty!
      (*client-tasks/fetching* *client-tasks/retry-fetching* *client-tasks/verifying* *client-tasks/retry-verifying*)
    (%prepare-fetching-quicklisp.sexp%)
    (%prepare-fetching-quicklisp-veriosn.sexp%)
    (dolist (meta metadata (values))
      (destructuring-bind (version . url) meta
        (format t "PREPARE CLIENT[~A] ......~%" version)
        (let* ((file (download-tmp-client-info.sexp version url :keep t))
               (info (read-from-string (uiop:read-file-string file))))
          (prepare-fetching version url :client-info info)
          (prepare-verifying version :client-info info)
          (write-new-client-info version info)
          (delete-tmp-downloaded-client-info file))))))

;;; -- 2nd -- Fetching all files from Quicklisp -------------------------------------------------
(defun mirror/client
    (&key (tasks/fetching *client-tasks/fetching*) (style *processing-style*)
       (tasks/retry-fetching *client-tasks/retry-fetching*) (filter #'identity))
  "Start mirroring by doing all tasks in *client-tasks/fetching*.
Before this step, you should make sure function 'initialize/client' successfully finished.
After this step, you should inspect whether all tasks finished in *client-tasks/retry-fetching*."
  (declare (type tasks-record tasks/fetching tasks/retry-fetching)
           (type (member :seq :parallel) style)
           (type function filter))
  (ensure-tasks-record-empty! (tasks/retry-fetching)
    (do-tasks-record tasks/fetching :container tasks/retry-fetching
                     :filter filter :style style)))

;;; -- 3rd -- Verification ----------------------------------------------------------------------
(defun verify/client
    (&key (tasks/verifying *client-tasks/verifying*) (style *processing-style*)
       (tasks/retry-verifying *client-tasks/retry-verifying*) (filter #'identity))
  "Start verifying by doing all tasks in *client-tasks/verifying.
Before this step, you should make sure either *client-tasks/fetching* or --
*client-tasks/retry-fetching* are finished.
After this step, you should inspect whether all tasks finished in *client-tasks/retry-verifying."
  (declare (type tasks-record tasks/verifying tasks/retry-verifying)
           (type (member :seq :parallel) style)
           (type function filter))
  (ensure-tasks-record-empty! (tasks/retry-verifying)
    (do-tasks-record tasks/verifying :container tasks/retry-verifying
                     :filter filter :style style)))
