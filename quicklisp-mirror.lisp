;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: QUICKLISP-MIRRO; Base: 10 -*-
;;;
;;; Copyright (C) 2012, 2017, 2018, 2019  Anthony Green <green@spindazzle.org>
;;;                         
;;; Quicklisp-Mirror is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.
;;;
;;; Quicklisp-Mirror is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Quicklisp-Mirro; see the file COPYING3.  If not see
;;; <http://www.gnu.org/licenses/>.

;; Top level for quicklisp-mirro

(in-package :quicklisp-mirror)

;; Our server....

(defvar *hunchentoot-server* nil)

(defvar *default-port-string* "80")

;; Start the web app.

(defun start-quicklisp-mirror (&rest interactive)
  "Start the web application and have the main thread sleep forever,
  unless INTERACTIVE is non-nil."
  (let ((openshift-port (sb-ext:posix-getenv "OPENSHIFT_PORT")))
    (let ((port (if openshift-port openshift-port *default-port-string*)))
      (format t "** Starting hunchentoot on ~A~%" port)
      (setq *hunchentoot-server* (hunchentoot:start 
				  (make-instance 'hunchentoot:easy-acceptor
						 :document-root #p"/opt/app-root/quicklisp/local-projects/quicklisp-mirror/"
						 :port (parse-integer port))))
      (if (not interactive)
	  (loop
	   (sleep 3000))))))

(defun stop-quicklisp-mirror ()
  "Stop the web application."
  (hunchentoot:stop *hunchentoot-server*))

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)

  (hunchentoot:define-easy-handler (status :uri "/health") ()
    (setf (hunchentoot:content-type*) "text/plain")
    (format nil "OK"))

  )

