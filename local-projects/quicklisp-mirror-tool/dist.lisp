;;;; dist.lisp
(in-package #:cl-user)
(defpackage #:quicklisp-mirror-tool.dist
  (:use #:cl
        #:split-sequence
        #:ql-mirror.variables
        #:ql-mirror.utils
        #:ql-mirror.task)
  (:nicknames #:ql-mirror.dist)
  (:export #:*dist-directory*
           #:*dist-quicklisp-directory*
           #:*dist-quicklisp-archive-directory*
           #:*dist-tasks/fetching*
           #:*dist-tasks/retry-fetching*
           #:*dist-tasks/retry-verifying*
           #:*releases*
           #:fetch-quicklisp.txt
           #:fetch-quicklisp-versions.txt
           #:initialize/dist
           #:mirror/dist
           #:mirror/releases
           #:verify/releases))
(in-package #:quicklisp-mirror-tool.dist)

;;; Global Variables
(defvar *dist-directory*
  (let ((directory* (uiop:directory-exists-p *web-server-directory-root*)))
    (unless directory*
      (error "Directory: ~A doesn't exist." *web-server-directory-root*))
    (merge-pathnames "dist/" (uiop:ensure-absolute-pathname directory*)))
  "The directory where all client information files would be put at.")

(defvar *dist-quicklisp-directory*
  (merge-pathnames "quicklisp/" *dist-directory*)
  "The direcotory of original \"quicklisp\" dist.")

(defvar *dist-quicklisp-archive-directory*
  (merge-pathnames "archive/quicklisp/" *web-server-directory-root*)
  "The directory of original \"quicklisp\" dist archives.")

(defvar *dist-tasks/fetching*
  (make-tasks-record :name "dist-tasks/fetching"
                     :capacity (+ 2 ;; fetch-quicklisp.txt + fetch-quicklisp-versions.txt
                                  ;; releases.txt and systems.txt
                                  (* 2 (length *available-dist-versions*))))
  "Tasks-Record for fetching dist files, including 'releases.txt' and 'systems.txt' for each version")

(defvar *dist-tasks/retry-fetching*
  (make-tasks-record :name "dist-tasks/retry-fetching")
  "Tasks-Record for retrying fetching broken (during downloading) files .")

(defvar *dist-tasks/retry-verifying*
  (make-tasks-record :name "dist-tasks/retry-verifying")
  "Tasks-Record for retrying fetching broken (during downloading) files .")

(defvar *releases* nil
  "All releases structs will be stored in this variable (as a list).")

;;; -- Utilities --------------------------------------------------------
;;; -- distinfo.txt -----------------------------------------------------
(defun parse-dist-info (url)
  "Given a dist-info string text (distinfo.txt), parse it to Lisp forms."
  (declare (type simple-string url))
  (with-open-stream (in (drakma:http-request url :want-stream t))
    (loop for line = (read-line in nil nil)
       while line
       collect (let* ((position-of-1st-colon (position #\: line :test 'char=))
                      (property (subseq line 0 position-of-1st-colon))
                      (value (subseq line (+ 2 position-of-1st-colon))))
                 (cons (alexandria:symbolicate property) value)))))

(defun modify-dist-info-hostname (dist-info)
  "Modify all urls' hostnames in a dist info."
  (dolist (thing dist-info dist-info)
    (case (car thing)
      ((|name| |version|) t)
      (t (setf (cdr thing) (modify-hostname (cdr thing)))))))

(defun write-dist-info (dist-info destination)
  "Given a dist-info, format it to be the original form then write it to disk."
  (declare (type cons dist-info)
           (type (or simple-string pathname) destination))
  (with-open-file (out destination :direction :output
                       :if-does-not-exist :create :if-exists :supersede)
    (dolist (line dist-info (values))
      (format out "~A: ~A~%" (car line) (cdr line)))))

(declaim (inline write-new-dist-info))
(defun write-new-dist-info (dist-info destination)
  (declare (type cons dist-info)
           (type (or simple-string pathname) destination))
  (write-dist-info (modify-dist-info-hostname dist-info) destination))

(declaim (inline make-dir-by-dist-version))
(defun make-dir-by-dist-version (version)
  "Given a version, e.g. \"2016-05-31\",
then transform it to #P\"/usr/local/var/www/dist/quicklisp/2016-05-31/\" and return."
  (declare (type simple-string version))
  (merge-pathnames (concatenate 'string version "/")
                   *dist-quicklisp-directory*))

(defun collect-required-urls (dist-info)
  "Find urls of systems.txt, releases.txt in a dist info."
  (mapcar (lambda (x)
            (cdr (assoc x dist-info :test 'eql)))
          '(|system-index-url|
            |release-index-url|)))

;;; -- releases.txt -----------------------------------------------------
(defstruct releases
  (version "" :type simple-string)
  (count 0 :type fixnum)
  (head-line "" :type simple-string)
  (content nil :type hash-table)
  (tasks/fetching nil :type tasks-record)
  (tasks/retry-fetching nil :type tasks-record)
  (tasks/verifying nil :type tasks-record)
  (tasks/retry-verifying nil :type tasks-record))

(defmethod print-object ((object releases) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (version count tasks/fetching tasks/retry-fetching tasks/verifying tasks/retry-verifying) object
      (format stream "Version: ~A, ~D releases in total.~%"
              version count)
      (dolist (record (list tasks/fetching tasks/retry-fetching tasks/verifying tasks/retry-verifying))
        (write-sequence (report-tasks-record-status record :output :string) stream)))))

(defun parse-releases.txt (version url)
  "Given the url of a releases.txt, parse it to a releases struct."
  (declare (type simple-string version url)) 
  (let ((count 0)
        (content (make-hash-table :test #+allegro '= #-allegro 'equal))
        head-line tasks/fetching tasks/retry-fetching tasks/verifying tasks/retry-verifying)
    (with-open-stream (in (drakma:http-request url :want-stream t))
      (setq head-line (read-line in nil nil))
      (loop for line = (read-line in nil nil)
         while line
         do (let ((line* (split-sequence #\Space line :test 'char= :remove-empty-subseqs t)))
              (setf (gethash (incf count) content) line*))))
    (setq tasks/fetching (make-tasks-record :name (format nil "releases-tasks/fetching, version ~A" version)
                                            :capacity count)
          tasks/retry-fetching (make-tasks-record :name (format nil "releases-tasks/retry-fetching, version ~A" version))
          tasks/verifying (make-tasks-record :name (format nil "releases-tasks/verifying, version ~A" version)
                                             :capacity count)
          tasks/retry-verifying (make-tasks-record :name (format nil "releases-tasks/retry-verifying, version ~A" version)))
    (make-releases :version version :count count :head-line head-line :content content
                   :tasks/fetching tasks/fetching :tasks/retry-fetching tasks/retry-fetching
                   :tasks/verifying tasks/verifying :tasks/retry-verifying tasks/retry-verifying)))

(defun prepare-releases (releases)
  (declare (type releases releases))
  (with-slots (version count content tasks/fetching tasks/verifying) releases
    (with-hash-table-iterator (next content)
      (loop 
         (multiple-value-bind (more-p key value) (next)
           (declare (ignore key))
           (unless more-p (return releases))
           (destructuring-bind (project url size md5 &rest args) value
             (declare (ignore size args))
             (let* ((destination (multiple-value-bind (url path) (modify-archive-url url)
                                   (declare (ignore url))
                                   (merge-pathnames (subseq path 1) *web-server-directory-root*))))
               (submit-task tasks/fetching
                            (make-task/fetching url destination
                                                :name (format nil "FETCHING ~A, DIST VERSION ~A" project version)
                                                :quiet t))
               (submit-task tasks/verifying
                            (make-task/verifying :file destination :md5 md5 :url url)))))))))

(defun write-releases.txt (releases destination)
  (declare (type releases releases)
           (type (or pathname simple-string) destination))
  (with-open-file (out destination :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
    (with-slots (count head-line content) releases
      (write-sequence head-line out) (terpri out)
      (loop for index from 1 to count
         do (let ((line* (gethash index content)))
              (setf (second line*) (modify-archive-url (second line*)))
              (format out "~{~A~^ ~}~%" line*))))))

;;; Funtions to prepare all tasks
;;; -- Utilities ----------------
(defun fetch-quicklisp.txt
    (&optional (url "http://beta.quicklisp.org/dist/quicklisp.txt"))
  "A default dist info file should be fetched; its link was given as 'distinfo-subscription-url'."
  (let ((quicklisp.txt (merge-pathnames "quicklisp.txt" *dist-directory*))
        (dist-info (parse-dist-info url)))
    (write-new-dist-info dist-info quicklisp.txt)
    (probe-file quicklisp.txt)))

(defun fetch-quicklisp-versions.txt
    (&optional (url "http://beta.quicklisp.org/dist/quicklisp-versions.txt"))
  "A defaut original quicklisp dist versions file should be fetched;
its link was defined by appending \"-versions\" to the name of subscription url."
  (let ((quicklisp-versions.txt (merge-pathnames "quicklisp-versions.txt" *dist-directory*))
        (versions (drakma:http-request url)))
    (with-open-file (out quicklisp-versions.txt :direction :output
                         :if-does-not-exist :create :if-exists :supersede)
      (with-input-from-string (in versions)
        (loop for line = (read-line in nil nil)
           while line do
             (let* ((position-of-space (position #\Space line :from-end t :test 'char=))
                    (version (string-trim '(#\Space) (subseq line 0 position-of-space)))
                    (url (subseq line (+ 1 position-of-space))))
               (format out "~A ~A~%" version (modify-hostname url))))))
    (probe-file quicklisp-versions.txt)))

(declaim (inline %prepare-fetching-quicklisp.txt% %prepare-fetching-quicklisp-veriosn.txt%))
(defun %prepare-fetching-quicklisp.txt% ()
  "Submit 'fetching quicklisp.txt' as a task/fetching to *dist-tasks/fetching*."
  (submit-task
   *dist-tasks/fetching*
   (make-task/fetching "http://beta.quicklisp.org/dist/quicklisp.txt"
                       (merge-pathnames "quicklisp.txt" *dist-directory*)
                       :name "FETCHIING QUICKLISP.TXT [DIST]"
                       :thunk #'fetch-quicklisp.txt)))
(defun %prepare-fetching-quicklisp-veriosn.txt% ()
  "Submit 'fetching quicklisp-versions.txt' as a task/fetching to *dist-tasks/fetching*."
  (submit-task
   *dist-tasks/fetching*
   (make-task/fetching "http://beta.quicklisp.org/dist/quicklisp-versions.txt"
                       (merge-pathnames "quicklisp-versions.txt" *dist-directory*)
                       :name "FETCHING QUICKLISP-VERSIONS.TXT [DIST]"               
                       :thunk #'fetch-quicklisp-versions.txt)))

(defun prepare-fetching (version dist-info)
  (let* ((urls (collect-required-urls dist-info))
         (system-url (first urls))
         (release-url (second urls))
         (dir (make-dir-by-dist-version version))
         (distinfo.txt (merge-pathnames "distinfo.txt" dir))
         (systems.txt (merge-pathnames "systems.txt" dir))
         (releases.txt (merge-pathnames "releases.txt" dir)))
    (ensure-directories-exist dir :verbose t)
    (submit-task *dist-tasks/fetching*
                 (make-task/fetching system-url systems.txt
                                     :name (format nil "FETCHING \"systems.txt\" for DIST[~A]" version)
                                     :quiet t))
    (submit-task *dist-tasks/fetching*
                 (make-task/fetching release-url releases.txt
                                     :name (format nil "FETCHING \"releases.txt\" for DIST[~A]" version)
                                     :thunk (lambda ()
                                              (let ((releases (parse-releases.txt version release-url)))
                                                (push (prepare-releases releases) *releases*)
                                                (write-releases.txt releases releases.txt)))))
    (write-new-dist-info dist-info distinfo.txt)))
    
;;; -- 1st -- Initialization. -----------------------------------------
(defun initialize/dist (&optional (metadata *available-dist-versions*))
  (ensure-tasks-record-empty!
      (*dist-tasks/fetching* *dist-tasks/retry-fetching*)
    (setf *releases* nil)
    (%prepare-fetching-quicklisp.txt%)
    (%prepare-fetching-quicklisp-veriosn.txt%)
    (dolist (meta metadata (values))
      (destructuring-bind (version . url) meta
        (format t "PREPARE DIST[~A] ......~%" version)
        (let ((dist-info (parse-dist-info url)))
          (prepare-fetching version dist-info))))
    *dist-tasks/fetching*))

;;; -- 2nd -- Fetching all files from Quicklisp ------------------------------------
(defun mirror/dist
    (&key (tasks/fetching *dist-tasks/fetching*) (style *processing-style*)
       (tasks/retry-fetching *dist-tasks/retry-fetching*) (filter #'identity))
  (ensure-tasks-record-empty! (*dist-tasks/retry-fetching*)
    (do-tasks-record tasks/fetching :container tasks/retry-fetching
                     :style style :filter filter))) 

(defun mirror/releases (&key (style *processing-style*))
  (ensure-tasks-record-empty! (*dist-tasks/retry-fetching*)
    (dolist (releases *releases* *dist-tasks/retry-fetching*)
      (with-slots (tasks/fetching tasks/retry-fetching) releases
        (do-tasks-record tasks/fetching :container tasks/retry-fetching :style style)
        (loop for task across (tasks-record-tasks tasks/retry-fetching)
           do (submit-task *dist-tasks/retry-fetching* task))))))

;;; -- 3rd -- Verification ----------------------------------------------------------
(defun verify/releases (&key (style *processing-style*))
  (ensure-tasks-record-empty! (*dist-tasks/retry-verifying*)
    (dolist (releases *releases* *dist-tasks/retry-verifying*)
      (with-slots (tasks/verifying tasks/retry-verifying) releases
        (do-tasks-record tasks/verifying :container tasks/retry-verifying :style style)
        (loop for task across (tasks-record-tasks tasks/retry-verifying)
           do (submit-task *dist-tasks/retry-verifying* task))))))
