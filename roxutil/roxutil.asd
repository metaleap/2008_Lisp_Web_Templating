(defpackage #:roxutil-asd
  (:use :cl :asdf))

(in-package :roxutil-asd)

(defsystem roxutil
  :name "roxutil"
  :version "0.1"
  :components ((:file "defpackage")
               (:file "pg-util" :depends-on ("defpackage"))
               (:file "main" :depends-on ("defpackage"))
               (:file "io" :depends-on ("defpackage")))
  :depends-on ())
