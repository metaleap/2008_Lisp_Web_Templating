(in-package :common-lisp-user)

(defpackage :roxbase
  (:nicknames :roxbase)
  (:use :cl :roxutil :hunchentoot)
  (:export
    ; template-tools
    set-parent-args set-root-args
    escape-for-html+ escape-for-js escape-for-safe expand+ expand+link set-parent-arg set-root-arg
    ; template-processor
    process-template process-template-inline process-template-inlines process-template-inlines-all
    ; main
    +inlex-placeholder+ +inlex-start-tag+ +inlex-end-tag+
    *chars-whitespace* *rox-escape-char* *rox-expand+linkers* *rox-expand+handlers* *rox-facade* *rox-inlex-handlers* *rox-server* *rox-settings*
    load-template
    create-args-table default-html-condition-handler facade-init get-state-hashtable handle-request server-startup server-shutdown))
