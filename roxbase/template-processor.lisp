(in-package :roxbase)

(defun process-template (template atts)
  (setf template
        (with-output-to-string (inlex-stream)
                               (process-template-inlines-all inlex-stream
                                                             (with-output-to-string (inmix-stream)
                                                                                    (process-template-inmixes inmix-stream template atts)) atts)))
  (if (and (null (search +inlex-start-tag+ template)) (null (search +inmix-start-tag+ template)))
      template
      (process-template template atts)))

(defun process-template-inline (stream atts identifier expr codeformat fn-eval fn-filter fn-html-condition-handler)
  (setf expr (subseq expr (1+ (length +inlex-start-tag+)) (- (length expr) (length +inlex-end-tag+))))
  (let ((exprlist #[*rox-inlex-cache* expr]))
    (macrolet ((try (condtype default action)
                 (let ((tmp-var (gensym)))
                   `(handler-case (let ((,tmp-var ,action)) ,(if default `(if ,tmp-var ,tmp-var ,default) tmp-var))
                                  (serious-condition (ex)
                                    (princ (funcall fn-html-condition-handler atts ,condtype expr (princ-to-string ex)) stream)
                                    (return-from process-template-inline NIL))))))
      (unless exprlist
        (let ((replaced (string-replace codeformat +inlex-placeholder+ expr)))
          (when fn-filter
              (funcall fn-filter stream replaced atts)
              (return-from process-template-inline NIL))
          (unless (find identifier #[*rox-settings* 'eval-compile-cache-handlers-p] :test #'equal)
            (princ replaced stream)
            (return-from process-template-inline NIL))
          (setf exprlist (mapcar #'(lambda (cexpr)
                                     (if (and (functionp cexpr) (not (compiled-function-p cexpr)))
                                         (try "COMPILE" NIL (compile NIL cexpr))
                                         cexpr)) (try "EVAL" NIL (eval-string replaced))))))
      (setf #[*rox-inlex-cache* expr] exprlist)
      (if (and fn-eval exprlist)
          (dolist (cexpr exprlist)
            (princ (try "RUNTIME" cexpr (funcall fn-eval cexpr atts)) stream))))))

(defun process-template-inlines-all (stream template atts)
  (let ((fn-html-condition-handler #[*rox-settings* 'fn-html-condition-handler]) (len (length template)) (len+1 (1+ *len-inlex-start*)) (len-1 (1- *len-inlex-end*)) (count 0) char)
    (dotimes (i len)
      (setf char (aref template i))
      (if (< count len+1)
          (if (or (>= count *len-inlex-start*) (char= char (aref +inlex-start-tag+ count)))
              (incf count)
              (if (= 0 count)
                  (write-char char stream)
                  (progn
                    (write-string template stream :start (- i count) :end (1+ i))
                    (setf count 0))))
          (if (string= +inlex-end-tag+ template :start2 (- i len-1) :end2 (1+ i))
              (let* ((handlerid (string (aref template (+ (- i count) *len-inlex-start*)))) (handler #[*rox-inlex-handlers* handlerid]))
                (if handler
                    (process-template-inline stream atts handlerid (string-displace template (- i count) NIL (1+ i)) (car handler) (cadr handler) (caddr handler) fn-html-condition-handler)
                    (princ (funcall fn-html-condition-handler atts "INLEX-HANDLER" (string-displace template (- i count) NIL (1+ i)) (if (string= " " handlerid) "You specified an inlex without a handler identifier." (concat "Unknown inlex handler identifier: " handlerid))) stream))
                (setf count 0))
              (incf count))))))

(defun process-template-inmix (stream atts expr)
  (let ((cexpr #[*rox-inmix-cache* expr]))
    (unless cexpr
      (setf cexpr (compile NIL (car (eval-string (process-template-inmix-build expr)))) #[*rox-inmix-cache* expr] cexpr))
    (princ (funcall cexpr atts) stream)))

(defun process-template-inmix-build (expr)
  (let ((symname (princ-to-string (gensym))))
    (string-nreplace
     (with-output-to-string (stream)
                            (princs* stream "#'(lambda (args) (proclaim '(optimize speed)) (in-package :" (server-name *rox-server*) ") (with-output-to-string (S" symname ") (let (T" symname ") ")
                            (process-template-inmix-build-core stream symname expr)
                            (write-string ")))" stream)) "(roxutil::progno )" "")))

(defun process-template-inmix-build-collect (expr)
  (let ((results (list)) (index 0) (len (length expr)) pos pos+lenstart pos2)
    (loop while (< index len) do
          (setf pos (search +inmix-start-tag+ expr :start2 index)
                pos+lenstart (+ pos *len-inmix-start*)
                pos2 (if pos (search +inmix-end-tag+ expr :start2 pos+lenstart) NIL))
          (if (and pos pos2)
              (progn
                (when (> pos index)
                  (setf results (nconc results (list (list NIL (string-displace expr index NIL pos))))))
                (setf results (nconc results (list (list T (string-displace expr pos+lenstart NIL pos2)))) index (+ pos2 *len-inmix-end*)))
              (setf results (nconc results (list (list NIL (string-displace expr index NIL)))) index len)))
    results))

(defun process-template-inmix-build-core (stream symname expr)
  (let ((results (process-template-inmix-build-collect expr)))
    (princ "(roxutil::progno " stream)
    (dolist (sublist results)
      (let* ((cadr-sublist (cadr sublist)) (char0 (aref cadr-sublist 0)))
        (if (car sublist)
            (if (char= #\= char0)
                (princs* stream "(if (setf T" symname " " (string-displace cadr-sublist 1 (1- (length cadr-sublist))) ") (princ T" symname " S" symname "))")
                (if (char= #\% char0)
                    (princs* stream "(princ \"%%" (string-displace cadr-sublist 1 (1- (length cadr-sublist))) "%%\")")
                    (princs* stream ") " cadr-sublist " (roxutil::progno ")))
            (princs* stream "(princ \"" (string-nreplace* cadr-sublist '(#\" "\\\"" "\\" "\\\\")) "\" S" symname ")"))))
    (princ #\) stream)))

(defun process-template-inmixes (stream template atts)
  (let* ((pos1 (search +inmix-start-tag+ template)) (pos2 (if pos1 (search +inmix-end-tag+ template :from-end T) NIL)) (both (and pos1 pos2)))
    (unless both
      (write-string template stream))
    (when both
      (let ((expr (make-array (+ *len-inmix-end* (- pos2 pos1)) :element-type 'character :adjustable NIL :displaced-to template :displaced-index-offset pos1)))
        (write-string template stream :start 0 :end pos1)
        (process-template-inmix stream atts expr)
        (write-string template stream :start (+ pos2 *len-inmix-end*))))))
