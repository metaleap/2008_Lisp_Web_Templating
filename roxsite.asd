;;;; 2008-07-15 18:37:30

(defpackage #:roxsite-asd
  (:use :cl :asdf))

(in-package :roxsite-asd)

(defsystem roxsite
  :name "roxsite"
  :version "0.1"
  :components ((:file "defpackage")
               (:file "main" :depends-on ("defpackage"))
               (:file "page" :depends-on ("defpackage")))
  :depends-on ())
