(in-package :roxutil)

(defun cons-file-append (filepath &rest cons)
  (with-output-file filepath #'cons-stream-append T cons))

(defun cons-file-append* (filepath &rest lists)
  (with-output-file filepath #'cons-stream-append* T lists))

(defun cons-file-select (filepath fn-pred)
  (with-input-file filepath #'cons-stream-select NIL fn-pred))

(defun cons-file-select* (filepath fn-pred &optional fn-handle)
  (with-input-file filepath #'cons-stream-select* NIL fn-pred fn-handle))

(defun cons-file-update* (filepath fn-pred fn-update)
  (let ((changed (with-input-file filepath #'cons-stream-update* NIL fn-pred fn-update)))
    (when changed
      (with-output-file filepath #'cons-stream-append* T changed))))

(defun cons-stream-append (stream &rest cons)
  (file-position stream :end)
  (write cons :stream stream)
  (write-char #\newline stream))

(defun cons-stream-append* (stream &rest lists)
  (dolist (cons lists)
    (write cons :stream stream)
    (write-char #\newline stream)))

(defun cons-stream-select (stream fn-pred)
  (loop for list = (read stream NIL) while list do
        (when (funcall fn-pred list)
          (return-from cons-stream-select list))))

(defun cons-stream-select* (stream fn-pred &optional fn-handle)
  (let ((file (if fn-handle NIL (list))) (null (null fn-pred)))
    (loop for list = (read stream NIL) while list do
          (when (or null (funcall fn-pred list))
            (progn 
              (if fn-handle
                  (funcall fn-handle list)
                  (setf file (nconc file (list list)))))))
    (unless fn-handle
      (when (car file) file))))

(defun cons-stream-update* (stream fn-pred fn-update)
  (let ((file (list)) (changed NIL))
    (loop for list = (read stream NIL) while list do
          (if (funcall fn-pred list)
              (setf file (if fn-update (nconc file (list (funcall fn-update list))) file) changed T)
              (nconc file (list list))))
    (when changed file)))

; Returns a string representing the entire contents of the specified stream.
(defun read-all-stream (stream &optional (prepend NIL) (append NIL))
  (with-output-to-string (out)
                         (when prepend (princ prepend out))
                         (loop for line = (read-line stream NIL) while line do (write-line line out))
                         (when append (princ append out))))

; Save the object denoted by val to the specified file denoted by filepath. If val is a list and walk-and-quote is
; given (and non-NIL), saves each list item prefixed with a quote (') character.
(defun save (val filepath &optional (walk-and-quote NIL))
  (with-open-file (stream filepath :direction :output :if-exists :supersede)
                  (if walk-and-quote
                      (dolist (item val)
                        (write-char #\' stream)
                        (write item :stream stream))
                      (write val :stream stream))))

; Opens the specified file denoted by filepath as a file input stream and applies the function
(defun with-input-file (filepath fn-stream rest &rest args)
  (with-open-file (stream filepath :direction :input :if-does-not-exist NIL)
                  (when stream
                    (apply fn-stream (nconc (list stream) (if rest (car args) args))))))

(defun with-output-file (filepath fn-stream rest &rest args)
  (with-open-file (stream filepath :direction :output :if-exists :append)
                  (apply fn-stream (nconc (list stream) (if rest (car args) args)))))
