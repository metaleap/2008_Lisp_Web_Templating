(in-package :roxbase)

(defmacro set-parent-args (args &rest argslist)
  `(apply* set-parent-arg (,args) "" ,@argslist))

(defmacro set-root-args (args &rest argslist)
  `(apply* set-root-arg (,args) "" ,@argslist))

(defun escape-for-html+ (string)
  (hunchentoot::escape-for-html string))

(defun escape-for-js (string)
  "Escapes a string so it can be put directly inside a JavaScript string."
  (string-replace* string (list #\newline "\\n" #\" "\\\"" #\< "&lt;" #\> "&gt;" #\' "&apos;")))

(defun escape-for-safe (&rest strings)
  "Returns a string representing the specified strings,
   joined by *rox-escape-char* and with *rox-escape-char*
   substituted for all non-alphanumeric characters."
  (with-output-to-string (stream)
                         (let ((not-first NIL))
                           (dolist (val strings)
                             (if not-first (princ *rox-escape-char* stream) (setf not-first T))
                             (princ (substitute-if-not *rox-escape-char* #'alphanumericp val) stream)))))

(defun expand+ (stream expr args) (declare (ignore args))
  (let* ((pos (string-index-of expr #'char-whitespace-p))
         (cmd (if pos (subseq expr 0 pos) expr))
         (rest (if pos (subseq expr pos) NIL))
         (handler #[*rox-expand+handlers* cmd])
         (handler-car (if (listp handler) (car handler) handler))
         (handler-cdr (if (listp handler) (cdr handler) NIL)))
    (if handler-car
        (apply handler-car (nconc (list args stream cmd rest) handler-cdr))
        (write-string (funcall #[*rox-settings* 'fn-html-condition-handler] args "EXPAND+" (concat #\+ expr) (concat "Unknown EXPAND+ command: '" cmd #\')) stream))))

(defun expand+concat (args stream cmd expr prepend append) (declare (ignore cmd))
  (princs* stream prepend expr append))

(defun expand+link (args stream cmd expr) (declare (ignore cmd))
  (setf expr (string-trim *chars-whitespace* expr))
  (let* ((pos (string-index-of expr #'char-whitespace-p))
         (cmd (if pos (subseq expr 0 pos) expr))
         (rest (if pos (eval-string (subseq expr pos)) NIL))
         (handler #[*rox-expand+linkers* cmd]))
    (if handler
        (dolist (val rest)
          (princs* stream (car handler) val (cadr handler)))
        (write-string (funcall #[*rox-settings* 'fn-html-condition-handler] args "EXPAND+LINK" expr (concat "Unknown EXPAND+LINK command: " cmd)) stream))))

(defun get-parent-arg (args name)
  #[(get-parent-args args) name])

(defun get-parent-args (args)
  #[args 'rox-parent-args])

(defun get-root-arg (args name)
  #[(get-root-args args) name])

(defun get-root-args (args)
  #[args 'rox-root-args])

(defun set-parent-arg (args name value)
  (setf #[#[args 'rox-parent-args] name] value))

(defun set-root-arg (args name value)
  (setf #[#[args 'rox-root-args] name] value))
