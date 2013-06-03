(in-package :roxbase)

(defvar +inlex-placeholder+ "%%INLEX%%")
(defvar +inlex-start-tag+ "<%")
(defvar +inlex-end-tag+ "%>")
(defvar +inmix-start-tag+ "<?")
(defvar +inmix-end-tag+ "?>")
(defvar *len-inlex-start* 0)
(defvar *len-inlex-end* 0)
(defvar *len-inmix-start* 0)
(defvar *len-inmix-end* 0)

(defvar *chars-whitespace* (concat #\space #\newline #\tab))
(defvar *rox-escape-char* #\_)
(defvar *rox-facade* NIL)
(defvar *rox-expand+linkers* (make-hash-table :test #'equal))
(defvar *rox-expand+handlers* (make-hash-table :test #'equal))
(defvar *rox-inlex-cache* (make-hash-table :test #'equal))
(defvar *rox-inlex-handlers* (make-hash-table :test #'equal))
(defvar *rox-inmix-cache* (make-hash-table :test #'equal))
(defvar *rox-output-cache* (make-hash-table :test #'equal))
(defvar *rox-server* NIL)
(defvar *rox-settings* (make-hash-table))

(defmacro load-template (args name &rest argslist)
  `(roxutil::princ* ,T (string-upcase ,name) #\newline)
  `(let ((atts (roxbase::create-args-table (gethash 'rox-rootpath ,args) (gethash 'rox-scripthandler ,args) ,args (gethash 'rox-root-args ,args))))
     ,(let ((setfer (list 'setf)))
        (dolist (pair argslist)
          (nconc setfer (list `(gethash ,(princ-to-string (car pair)) atts) (cadr pair))))
        setfer)
     (handle-request ,name atts T)))

(defun create-args-table (pathname-root script-path-handler parent-args root-args)
  (let ((args (make-hash-table :test #'equal)))
    (populate-hash-table args
                         ('rox-rootpath pathname-root)
                         ('debug *show-lisp-errors-p*)
                         ('rox-scripthandler script-path-handler))
    (if parent-args
        (setf #[args 'rox-parent-args] parent-args))
    (if root-args
        (setf #[args 'rox-root-args] root-args)
        (setf #[args 'rox-root-args] args))
    args))

(defun default-html-condition-handler (args condtype condsrc condmsg) (declare (ignore args))
  (let ((condname (escape-for-safe condtype condsrc condmsg)))
    (concat "<a style='text-decoration: none;' href='#" condname "' name='" condname "' onclick='alert(\"" (escape-for-js (concat condtype " condition at:" #\newline condsrc #\newline #\newline condmsg)) "\");'><pre style='background-color: buttonface; color: red; display: inline; font-size: small; margin: 2px;'>" (escape-for-html condsrc) "</pre></a>")))

(defun facade-init (foldername)
  (princ* T "facade-init: " foldername)
  (set-dispatch-macro-character #\# #\f #'(lambda (stream char1 char2)
                                            (declare (ignore char1 char2))
                                            (let ((sym (read stream T NIL T)))
                                              `(string-nreplace ,sym "/~/" (concat "/" (if ,foldername ,foldername #\!) "/" *rox-facade* "/"))))))

(defun get-state-hashtable (key)
  (let* ((state-hashtables #[*rox-settings* 'state-hashtables]) (ht #[state-hashtables key]))
    (unless ht
      (setf #[state-hashtables key] (setf ht (make-hash-table :test #'equal)))
      (setf #[ht "__key"] key))
    ht))

(defun handle-request (script-name args subtemplate-p)
  (let* ((script-path-handler #[args 'rox-scripthandler]) (pathname-root #[args 'rox-rootpath]) (script-path (funcall script-path-handler script-name args subtemplate-p)) (realpath (if (char= #\/ (char script-path 0)) script-path (concat pathname-root script-path))))
    (with-open-file (mystream realpath :if-does-not-exist NIL)
                    (unless mystream
                      (return-from handle-request (funcall #[*rox-settings* 'fn-html-condition-handler] args "404" realpath "404 Not Found")))
                    (setf #[args 'rox-statetable] (get-state-hashtable (script-name)))
                    (process-template (read-all-stream mystream (concat +inlex-start-tag+ "% " +inlex-end-tag+)) args))))

(defun server-startup (name port pathname-root script-path-handler &key (foldername-static "static") (debug NIL) (scriptpackage T))
  (setf *len-inlex-start* (length +inlex-start-tag+) *len-inlex-end* (length +inlex-end-tag+) *len-inmix-start* (length +inmix-start-tag+) *len-inmix-end* (length +inmix-end-tag+))
  (if (eq T scriptpackage)
      (setf scriptpackage name))
  (if port
      (setf *show-lisp-errors-p* debug *show-lisp-backtraces-p* debug))
  (populate-hash-table *rox-expand+linkers*
                       ("javascript" "<script type=\"text/javascript\" language=\"JavaScript\" src=\"" "\"></script>")
                       ("stylesheet" "<link rel=\"stylesheet\" type=\"text/css\" href=\"" "\" />"))
  (populate-hash-table *rox-expand+handlers*
                       ("javascript" #'expand+concat "<script type=\"text/javascript\" language=\"JavaScript\"> // <![CDATA[" " // ]]></script>")
                       ("stylesheet" #'expand+concat "<style type=\"text/css\">" "</style>")
                       ("link" #'expand+link))
  (populate-hash-table *rox-inlex-handlers*
                       ("*" (concat +inlex-start-tag+ +inlex-placeholder+ +inlex-end-tag+) NIL #'(lambda (stream expr args)
                                                                                                   (let ((cached #[*rox-output-cache* expr]))
                                                                                                     (unless cached
                                                                                                       (setf cached (process-template expr args) #[*rox-output-cache* expr] cached))
                                                                                                     (write-string cached stream))))
                       ("!" "" NIL)
                       ("^" (concat +inlex-start-tag+ "= (set-parent-args args " +inlex-placeholder+ ") " +inlex-end-tag+) NIL)
                       ("~" (concat +inlex-start-tag+ "% (mapcar #'(lambda (val) (load (if (char= #\\/ (char val 0)) val (roxutil::concat \"" pathname-root "\" val)))) (list " +inlex-placeholder+ ")) " +inlex-end-tag+) NIL)
                       ("%" (concat (if scriptpackage (concat "(in-package :" scriptpackage ")" #\newline) "") ";(proclaim '(optimize speed))" #\newline +inlex-placeholder+) NIL)
                       ("@" (concat +inlex-start-tag+ "= (gethash " +inlex-placeholder+ " args) " +inlex-end-tag+) NIL)
                       ("'" (concat +inlex-start-tag+ "= (roxbase::escape-for-js " +inlex-placeholder+ ") " +inlex-end-tag+) NIL)
                       (":" (concat +inlex-start-tag+ "= (roxbase::escape-for-safe " +inlex-placeholder+ ") " +inlex-end-tag+) NIL)
                       ("#" (concat +inlex-start-tag+ "= (roxbase::load-template args " +inlex-placeholder+ ") " +inlex-end-tag+) NIL)
                       ("=" (concat "(lambda (args) (let ((tmp " +inlex-placeholder+ ")) (if tmp (princ-to-string tmp) \"\")))") #'funcall)
                       ("+" +inlex-placeholder+ NIL #'expand+))
  (if port
      (populate-hash-table *rox-inlex-handlers*
                           ("\"" (concat +inlex-start-tag+ "= (hunchentoot::escape-for-html " +inlex-placeholder+ ") " +inlex-end-tag+) NIL)
                           ("?" (concat +inlex-start-tag+ "= (hunchentoot::get-parameter " +inlex-placeholder+ ") " +inlex-end-tag+) NIL)
                           ("&" (concat +inlex-start-tag+ "= (hunchentoot::post-parameter " +inlex-placeholder+ ") " +inlex-end-tag+) NIL)
                           ("$" (concat +inlex-start-tag+ "= (hunchentoot::session-value " +inlex-placeholder+ ") " +inlex-end-tag+) NIL)
                           ("|" (concat +inlex-start-tag+ "= (hunchentoot::header-in " +inlex-placeholder+ ") " +inlex-end-tag+) NIL)))
  (populate-hash-table *rox-settings*
                       ('rox-rootpath pathname-root)
                       ('fn-html-condition-handler #'default-html-condition-handler)
                       ('scriptpackage scriptpackage)
                       ('eval-compile-cache-handlers-p "%" "=")
                       ('state-hashtables (make-hash-table :test #'equal)))
  (if port
      (setf hunchentoot::*rewrite-for-session-urls* NIL *rox-server* (start-server
                          :name name
                          :port port
                          :dispatch-table (list
                                           (create-folder-dispatcher-and-handler (concat "/" foldername-static "/") (concat pathname-root foldername-static "/"))
                                           (create-prefix-dispatcher "/" #'(lambda ()
                                                                             (let* ((script-name (script-name)) (output (time (handle-request script-name (create-args-table pathname-root script-path-handler NIL NIL) NIL))))
                                                                               (if (string= "/rss/" script-name) (concat "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>" output) output)))))))))

(defun server-shutdown ()
  (if *rox-server*
      (setf *rox-server* (stop-server *rox-server*)))
  (setf *rox-facade* NIL)
  (dolist (ht (list *rox-expand+linkers* *rox-output-cache* *rox-expand+handlers* *rox-inlex-cache* *rox-inmix-cache* *rox-inlex-handlers* *rox-settings*))
    (clrhash ht)))

(roxbase::facade-init NIL)
