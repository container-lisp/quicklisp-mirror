;;;; task.lisp
(in-package #:cl-user)
(defpackage #:quicklisp-mirror-tool.task
  (:use #:cl
        #:ql-mirror.variables
        #:ql-mirror.utils)
  (:nicknames #:ql-mirror.task)
  (:export
   ;; -- classes ----------------------------------------
   #:task
   #:task/fetching #:task/retry-fetching
   #:task/verifying #:task/retry-verifying
   ;; -- constructors, slots and accessors --------------
   #:make-task #:make-task/fetching #:make-task/verifying
   #:index #:name #:thunk #:status
   #:url #:destination #:file #:md5
   #:task-index #:task-name #:task-thunk #:task-status
   ;; -- methods ----------------------------------------
   #:copy-task #:run-task #:submit-task
   ;; -- tasks-record and its APIs ----------------------
   #:tasks-record #:fixed-length-p #:tasks
   #:make-tasks-record #:empty-tasks-record!
   #:report-tasks-record-status
   #:tasks-record-capacity #:tasks-record-tasks
   #:ensure-tasks-record-empty! #:submit-task 
   #:count-tasks-by-status #:collect-tasks-from-record
   #:failed-task-on-record-p #:collect-failed-from-record 
   #:do-tasks-record
   ;; -- end of export ----------------------------------
   ))
(in-package #:quicklisp-mirror-tool.task)

;;; Task
(defclass task ()
  ((index :documentation "The index of a task, which is an uninterned symbol."
          :accessor task-index :type symbol)
   (name :documentation "The name of a task."
         :initarg :name :accessor task-name :type simple-string)   
   (thunk :documentation "A task can be issued by funcalling this thunk, which is a function (or lambda) without any arguments."
          :initarg :thunk :accessor task-thunk :type function)
   (status :documentation "Task's status."
           :initarg :status :accessor task-status :type keyword))
  (:documentation "A task is an abstraction model for helping users managing and --
tracing successfully finished or accidentaly failed jobs."))

(declaim (inline make-task))
(defun make-task (&key (name (symbol-name (gensym "ANONYMOUS-TASK-")))
                    (thunk (lambda () niL)))
  "Construction funtion for task object."
  (make-instance 'task :name name :thunk thunk))

(defmethod initialize-instance
    :before ((instance task) &rest initargs &key &allow-other-keys)
  "Automatically allocate indices for task instances."
  (declare (ignore initargs))
  (with-slots (index status) instance
    (setf index (gensym "TASK-")
          status :initialized)))

(defmethod print-object ((object task) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (terpri stream)
    (format stream "	Name: ~S~%	Status: ~A~%"
            (task-name object) (task-status object))))

;;; Task -> task/retry
(defclass task/retry (task) nil
  (:documentation "An abstraction around those failed tasks which may need to be retried again."))

;;; Task -> task/fetching
(defclass task/fetching (task)
  ((url :documentation "The source url for fetching."
        :initarg :url :accessor url :type simple-string)
   (destination :documentation "The destination for fetching, a pathname or a string representing pathspec."
                :initarg :destination :accessor destination :type (or simple-string pathname)))
  (:documentation "An abstraction around fetching resources from remote."))

(defun make-task/fetching (url destination &key (name nil name-p) (thunk nil thunk-p) (quiet t))
  "Construction function for task/fetching object.
If thunk is not given, then using an anonymous lambda applying 'trivial-download:download' to url, destination and quiet."
  (declare (ignorable url destination))
  (make-instance 'task/fetching
                 :name (if name-p 
                           name
                         (format nil "FETCHING ~S" url))
                 :thunk (if thunk-p
                            thunk
                          (lambda () (trivial-download:download url destination :quiet quiet)))
                 :url url :destination destination))

(defmethod print-object :after ((object task/fetching) stream)
  (format stream "~&NOTE: FETCHING from ~S to ~S.~%"
          (url object) (destination object)))

;;; Task -> task/fetching -> task/retry-fetching
;;;      -> task/retry -> task/retry-fetching
(defclass task/retry-fetching (task/retry task/fetching) nil
  (:documentation "An abstraction around those failed fetching tasks."))

(defmethod print-object :after ((object task/retry-fetching) stream)
  (with-slots (url destination) object
    (format stream "~A: retry fetching from ~S to ~S.~%"
            (task-status object) url destination)))

;;; Task -> task/verifying
(defclass task/verifying (task)
  ((file :documentation "It denotes the file to be verified."
         :initarg :file :accessor file :type (or pathname simple-string))
   (md5 :documentation "A given md5 string to do the verification."
        :initarg :md5 :accessor md5 :type simple-string)
   (url :documentation "An url telling where the @file was fetched from.
When a verification was failed, this url will be used to fetch it again."
        :initarg :url :accessor url :type simple-string))
  (:documentation "An abstraction around verifying fetched files."))

(defun make-task/verifying (&key file md5 url)
  (make-instance 'task/verifying
                 :name (format nil "VERIFYING ~S" file)
                 :thunk (lambda () (verify-file-md5 file md5))
                 :file file :md5 md5 :url url))

(defmethod print-object :after ((object task/verifying) stream)
  (with-slots (file md5) object
    (format stream "~&NOTE: VERIFYING ~S by MD5[~A]~%" file md5)))

;;; Task -> task/verifying -> task/retry-verifying
;;;      -> task/retry -> task/retry-verifying
(defclass task/retry-verifying (task/retry task/verifying) nil
  (:documentation "An abstraction around those failed fetching tasks."))

(defmethod print-object :after ((object task/retry-verifying) stream)
  (with-slots (status file) object
    (format stream "~A: retry verifying ~S.~%"
            status file)))
                 
;;; Tasks Record
(defstruct (tasks-record (:constructor %make-tasks-record%))
  "A tasks record maintains the information of a vector of tasks."
  (name "" :type simple-string)
  (fixed-length-p nil :type symbol)
  (%capacity% 0 :type (or symbol fixnum))
  (tasks #() :type (vector task *)))

(defun make-tasks-record (&key (name "TASKS-RECORD") (capacity 0) displaced-to)
  "Construction function for tasks-record.
If 'capacity' is zero, then create an array with :adjustable to be t;
otherwise the capacity of record is fixed, which means the size of vector is not adjustable.
If 'displaced-to' is applied and capacity is not zero, then also apply its value to 'make-array'."
  (declare (ignorable capacity displaced-to))
  (if (zerop capacity)
      (%make-tasks-record% :name name
       :fixed-length-p nil :%capacity% :dynamic
       :tasks (make-array 0 :element-type 'task :fill-pointer 0 :adjustable t))
      (%make-tasks-record% :name name
       :fixed-length-p t :%capacity% capacity
       :tasks (make-array capacity :element-type 'task :displaced-to displaced-to
                          :fill-pointer 0 :adjustable nil))))

(defun report-tasks-record-status (record &key (output :list))
  (declare (type tasks-record record)
           (type (member :list :string) output))
  (with-slots (name tasks) record
    (let ((stats (mapcar (lambda (status)
                           (cons status (count-tasks-by-status record status)))
                         (delete-duplicates (map 'list 'task-status tasks) :test 'eq))))
      (case output
        (:list stats)
        (:string
         (let* ((capacity (tasks-record-capacity record))
                (status-info (mapcar (lambda (cell)
                                       (format nil "~d/~d tasks ~A"
                                               (cdr cell) capacity (car cell)))
                                     stats)))
           (if stats
               (format nil "[~a]: ~d tasks TOTAL, ~{~A~^, ~}.~%" name capacity status-info)
               (format nil "[~a]: ~d tasks TOTAL.~%" name capacity))))))))
             

(defmethod print-object ((object tasks-record) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write-sequence (report-tasks-record-status object :output :string) stream)))

;;; Generic Functions
(defgeneric copy-task (task &key identity)
  (:documentation "Copy a task instance. If identity is true, then also copy the index slot; allocate an new index otherwise."))

(defgeneric run-task (task &key &allow-other-keys)
  (:documentation "Run a task and return its result if no errors;
any implementation should at least wrap around any potential error and
modify its status according to those errors."))

(defgeneric submit-task (record task &rest args)
  (:documentation "Submit a task to a tasks record.
The task parameter may either be a task or a function. When it is a function,
it will make an anonymous task where its thunk is made up by apply the function to rest arguments."))

;;; --- Methods -----------------------------------
;;; --- copy-task ---------------------------------
(defmethod copy-task ((task task) &key identity)
  (declare (type (member t nil) identity))
  (with-slots (index name thunk status) task
    (if identity
        (make-instance 'task :index index
                       :name name :thunk thunk :status status)
      (make-task :name name :thunk thunk :status status))))

(defmethod copy-task ((task task/fetching) &key identity)
  (with-slots (name thunk status index url destination) task
    (let ((copy (make-task/fetching url destination
                                    :name name
                                    :thunk thunk)))
      (setf (task-status copy) status)
      (when identity
        (setf (task-index copy) index))
      copy)))

(defmethod copy-task ((task task/verifying) &key identity)
  (with-slots (index status file md5 url) task
    (let ((copy (make-task/verifying :file file :md5 md5 :url url)))
      (setf (task-status task) status)
      (when identity
        (setf (task-index copy) index))
      copy)))

;;; --- run-task ----------------------------------
(defmethod run-task ((task task) &key)
  "This method could be seen as acting the 'default' behavior of run-task.
If processing this task is successful, then return the result of calling thunk;
otherwise, prompt a warning information and then set the status of task to be :failed."
  (with-slots (name thunk status) task
    (handler-case
        (let ((result (funcall thunk)))
          (setf status :finished)
          result)
      (error (c)
        (warn "~&Due to ~S~%Task ~A failed.~&" c name)
        (setf status :failed)))))

(defmethod run-task ((task task/fetching) &key (container nil container-p))
  "Try fetching, if error happens, submit this task to a tasks-record which --
should hold a group of failed fetching tasks. Finally returns the task itself anyway."
  (declare (type tasks-record container))
  (with-slots (thunk status url) task
    (handler-case (progn
                    (funcall thunk)
                    (setf status :finished))
      (error (c)
        (warn "Due to ~S~&Fetching from ~S failed.~&" c url)
        (setf status :failed)
        (when container-p
          (let ((task-copy (copy-task task :identity t)))
            (change-class task-copy 'task/retry-fetching)
            (submit-task container task-copy)))))
    task))

(defmethod run-task ((task task/retry-fetching) &key)
  (with-slots (thunk status url) task
    (handler-case (progn
                    (funcall thunk)
                    (setf status :finished))
      (error (c)
        (warn "Due to ~S~&Retry fetching from ~S still failed.~&" c url)
        (setf status :retrying)))
    task))

(defmethod run-task ((task task/verifying) &key (container nil container-p))
  (with-slots (thunk status file url) task
    (handler-case (progn (funcall thunk)
                         (setf status :finished)) 
      (error (c)
        (warn "Due to ~S~&Verifying ~S failed.~%" c file)
        (setf status :failed)
        (when container-p
          (let ((task-copy (copy-task task :identity t)))
            (change-class task-copy 'task/retry-verifying)
            (submit-task container task-copy)))))
    task))

(defmethod run-task ((task task/retry-verifying) &key)
  (with-slots (thunk status file url) task
    (handler-case (progn (trivial-download:download url file :quiet nil)
                         (funcall thunk)
                         (setf status :finished))
      (error (c)
        (warn "Due to ~S~&Verifying ~S still failed.~%" c file)
        (setf status :retrying)))
    task))

;;; --- submit-task --------------------------------
(defmethod submit-task :before (record (task task) &rest args)
  (declare (ignore record args))
  (setf (task-status task) :submitted))

(defmethod submit-task (record (task task) &rest args)
  (declare (type tasks-record record)
           (ignore args))
  (with-slots (fixed-length-p tasks) record
    (funcall (if fixed-length-p 'vector-push 'vector-push-extend)
             task tasks)
    (values record (tasks-record-capacity record))))

(defmethod submit-task :around (record (task task/retry) &rest args)
  (declare (type tasks-record record)
           (ignore args))
  (unless (position task (tasks-record-tasks record) 
                    :key 'task-index :test 'eq)
    (call-next-method)))

(defmethod submit-task (record (fn function) &rest args)
  "Using a function applied with the 'args' to submit an anonymous task."
  (declare (type tasks-record record))
  (with-slots (fixed-length-p tasks) record
    (let ((task (make-task :thunk (lambda () (apply fn args)))))
      (funcall (if fixed-length-p 'vector-push 'vector-push-extend)
               task tasks)
      (values record (tasks-record-capacity record)))))

;;; Tasks Record APIs
(defun tasks-record-capacity (tasks-record)
  "Return a tasks record's (current) capacity."
  (declare (type tasks-record tasks-record))
  (with-slots (fixed-length-p %capacity% tasks) tasks-record
    (declare (ignorable %capacity%))
    (if fixed-length-p
        %capacity%
        (fill-pointer tasks))))

(defun empty-tasks-record! (record)
  "Empty (destructively, maybe) a tasks record."
  (with-slots (fixed-length-p tasks %capacity%) record
    (when (zerop (tasks-record-capacity record))
      (return-from empty-tasks-record! record))
    (setf tasks
          (if fixed-length-p
              (make-array %capacity% :element-type 'task 
                          :fill-pointer 0 :adjustable nil)
              (make-array 0 :element-type 'task 
                          :fill-pointer 0 :adjustable t)))
    record))

(defmacro ensure-tasks-record-empty! ((&rest records) &body body)
  "Ensure the record(s) is empty (by using 'empty-tasks-record!', which is destructive) then evaluate forms in body."
  `(progn
     ,@(loop for record in records
          collect `(empty-tasks-record! ,record))
     ,@body))

(defun count-tasks-by-status (tasks-record status)
  "Count how many tasks by matching the status."
  (declare (type tasks-record tasks-record)
           (type keyword status))
  (with-slots (tasks) tasks-record
    (count-if (lambda (task)
                (eq (task-status task) status))
              tasks)))

(defun failed-task-on-record-p (tasks-record)
  (declare (type tasks-record tasks-record))
  (with-slots (tasks) tasks-record
    (and (find-if (lambda (x) (eq x :failed))
                  tasks :key 'task-status)
         t)))

(defun collect-tasks-from-record (tasks-record filter)
  "Collect (non-destructive) tasks from a tasks-record by a filter function (predicate), and put them into a new tasks record."
  (declare (type tasks-record tasks-record)
           (type function filter))
  (with-slots (name tasks) tasks-record
    (let ((new-tasks (remove-if (complement filter) tasks)))
      (make-tasks-record :name name
                         :capacity (length new-tasks)
                         :displaced-to new-tasks))))

(defun collect-failed-from-record (tasks-record)
  "Collect failed tasks from a record and put them into a new tasks record struct."
  (declare (type tasks-record tasks-record))
  (collect-tasks-from-record
   tasks-record (lambda (task) (eq (task-status task) :failed))))

(declaim (inline current-time))
(defun current-time ()
  (multiple-value-bind (sec min hr) (get-decoded-time)
    (when (< sec 10) (setq sec (format nil "0~d" sec)))
    (when (< min 10) (setq min (format nil "0~d" min)))
    (when (< hr 10) (setq hr (format nil "0~d" hr)))
    (format nil "~a:~a:~a" hr min sec)))

(defun do-tasks-record
    (record &key (container nil container-p) (filter #'identity) (style *processing-style*))
  (declare (type tasks-record record)
           (type (or symbol tasks-record) container)
           (type function filter)
           (type (member :seq :parallel) style)
           (optimize speed (safety 0) (space 0) (debug 0) (compilation-speed 0)))
  (let ((total (count-if filter (tasks-record-tasks record)))
        (count 0)
        (fmt-control "~&[~d/~d] ~A processed in ~A~&")
        (out *standard-output*))
    (format out "Start processing ~A~%" (tasks-record-name record))
    (flet ((do-task (task)
             (prog1 (if container-p
                        (run-task task :container container)
                        (run-task task))
               (case style
                 (:seq 
                  (format out fmt-control
                          (incf count) total (task-name task) (current-time)))
                 (:parallel ;; :parallel 
                  (bt:with-recursive-lock-held (*tasks-record-lock*)
                    (format out fmt-control
                            (incf count) total (task-name task) (current-time))))))))
      (with-slots (tasks) record
        (case style
          (:seq (loop for task across tasks
                   do (do-task task)))
          (t (let ((lparallel:*kernel*
                    (lparallel:make-kernel *number-of-workers*
                                           :name (symbol-name (gensym "TASKS-RECORD-")))))
               (lparallel:pmap nil #'do-task tasks))))))
    (format out "~&Processing ~A finished.~%" (tasks-record-name record))
    record))
