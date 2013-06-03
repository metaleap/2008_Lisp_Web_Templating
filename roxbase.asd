(defpackage #:roxbase-asd
  (:use :cl :asdf))

(in-package :roxbase-asd)

(defsystem roxbase
  :name "roxbase"
  :version "0.1"
  :components ((:file "defpackage")
               (:file "template-processor" :depends-on ("defpackage"))
               (:file "template-tools" :depends-on ("defpackage"))
               (:file "main" :depends-on ("defpackage")))
  :depends-on (roxutil))

(require 'asdf)
(pushnew :hunchentoot-no-ssl *features*)
(asdf::oos 'asdf::load-op :hunchentoot-test)
