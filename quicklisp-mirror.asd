;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Copyright (C) 2012, 2019  Anthony Green <green@spindazzle.org>
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
;;; along with Quicklisp-Mirror; see the file COPYING3.  If not see
;;; <http://www.gnu.org/licenses/>.

(asdf:defsystem #:quicklisp-mirror
  :description "This is my quicklisp-mirror template."
  :author "Anthony Green <green@spindazzle.org>"
           :version "0"
  :serial t
  :components ((:file "package")
	       (:file "quicklisp-mirror"))
  :depends-on (:hunchentoot))

