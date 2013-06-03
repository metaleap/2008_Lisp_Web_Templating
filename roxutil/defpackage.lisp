(in-package :common-lisp-user)

(defpackage :roxutil
  (:nicknames :roxutil)
  (:use :cl)
  (:export
    ; main macros
    apply* fun populate-hash-table princ* princs*
    ; main funcs
    char-whitespace-p clrhash* concat day-of-week defdelim eval-file eval-stream eval-string in-p join string-displace string-empty-p string-ends string-index-of string-join string-nreplace string-nreplace* string-nreplace-reduce string-replace string-replace* string-replace-reduce string-starts string-sub
    ; io funcs
    cons-file-append cons-file-append* cons-file-select cons-file-select* cons-file-update* cons-stream-append cons-stream-append* cons-stream-select cons-stream-select* cons-stream-update* read-all-stream save with-input-file with-output-file
    ; pg-util macros
    do-for do-until do-while if-let when-let
    ; pg-util funcs
    append1 memoize
    ))
