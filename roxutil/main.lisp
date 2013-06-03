(in-package :roxutil)

; Expands into as many calls to the specified function denoted by fn (not #'fn!) as there are sub-lists in the specified
; list denoted by variable-args and returns the value denoted by val. Each call to fn always first passes all values in
; the specified list denoted by constant-args before passing the variable-args.
(defmacro apply* (fn constant-args val &rest variable-args)
  (let ((setters (list 'progn)))
    (dolist (pair variable-args)
      (setf setters (nconc setters (list `(,fn ,@constant-args ,@pair)))))
    (setf setters (nconc setters (list val)))))

; Generates a single (setf) call that populates the hash-table denoted by ht with all the key-value pairs denoted by
; pairs. Each key-value pair is a list. Example: (populate-hash-table myht ('one 1) ('two 2))
(defmacro populate-hash-table (ht &rest pairs)
  (let ((setfer (list 'setf)))
    (dolist (pair pairs)
      (let* ((cdr-pair (cdr pair)) (len (length cdr-pair)))
        (nconc setfer (list `(gethash ,(car pair) ,ht) (if (= len 1) (car cdr-pair) `(list ,@cdr-pair))))))
    setfer))

; Generates a series of (princ) calls embedded in a (progn) block, one per argument in the vals &rest list. Each (princ)
; call targets the specified stream denoted by stream.
(defmacro princ* (stream &rest vals)
  (let ((princers (list 'progn)))
    (dolist (val vals princers)
      (if (and (listp val) (equal (car val) 'subseq))
          (nconc princers (list (list 'write-string (cadr val) stream ':start (caddr val) :end (cadddr val))))
          (nconc princers (list (list 'princ val stream)))))))

; Generates a series of (write-string) calls embedded in a (progn) block, one per argument in the vals &rest list.
; Each (write-string) call targets the specified stream denoted by stream.
(defmacro princs* (stream &rest vals)
  (let ((princers (list 'progn)))
    (dolist (val vals princers)
      (if (and (listp val) (equal (car val) 'subseq))
          (nconc princers (list (list 'write-string (cadr val) stream ':start (caddr val) :end (cadddr val))))
          (nconc princers (list (list 'write-string val stream)))))))

; If the specified &body consists of exactly one s-exp, it is expanded; otherwise, body is expanded into a (progn).
(defmacro progno (&body body)
  (if (or (null body) (cadr body))
      `(progn ,@body)
      `(,@(car body))))

; Shifts all elements in the specified array denoted by arr (of the specified length denoted by len) one position to
; the left and destructively puts the specified value denoted by val into arr at the last position, then returns arr.
(defun array-shift-append (arr val len)
  (setf (aref (dotimes (i (1- len) arr)
                (setf (aref arr i) (aref arr (1+ i)))) (1- len)) val)
  arr)

; Shifts all elements in the specified array denoted by arr (of the specified length denoted by len) one position to the
; right and destructively inserts the specified value denoted by val into arr at the first position, then returns arr.
(defun array-shift-prepend (arr val len)
  (loop for i from (1- len) downto 1 collect i do
        (setf (aref arr i) (aref arr (1- i))))
  (setf (aref arr 0) val)
  arr)

; Returns T if the specified character denoted by char equals the #\space, #\newline, #\tab or #\return whitespace
; characters; otherwise, returns NIL.
(defun char-whitespace-p (char)
  (or (char= #\space char) (char= #\newline char) (char= #\tab char) (char= #\return char)))

; Clears all the specified hash-tables.
(defun clrhash* (&rest hash-tables)
  (dolist (ht hash-tables)
    (clrhash ht)))

; Builds a new string containing all the (princ) representations of the arguments specified in the vals &rest list."
(defun concat (&rest vals)
  (with-output-to-string (stream)
                         (dolist (val vals)
                           (typecase val
                                     (character (write-char val stream))
                                     (string (write-string val stream))
                                     (T (princ val stream))))))

; Returns a copy of the specified value denoted by val if it is a list, a sequence, a structure object or a symbol;
; otherwise, returns val.
(defun copy (val)
  (typecase val
            (list (copy-list val))
            (sequence (copy-seq val))
            (structure-object (copy-structure val))
            (symbol (copy-symbol val T))
            (T val)))

(defun day-of-week (day month year)
  (nth-value 6 (decode-universal-time (encode-universal-time 0 0 0 day month year 0) 0)))

; Defines a reader macro to invoke the specified function denoted by fn for all s-expressions within the delimiting
; (dispatch) characters denoted by lchar and rchar."
(defun defdelim (lchar rchar fn)
  (set-macro-character rchar (get-macro-character #\)))
  (set-dispatch-macro-character #\# lchar #'(lambda (stream char1 char2)
                                              (declare (ignore char1 char2))
                                              (funcall fn (read-delimited-list rchar stream T)))))

; Reads and evaluates each s-expression contained in the specified file and returns a list containing all evaluation
; results.
(defun eval-file (path default)
  (with-open-file (stream path :if-does-not-exist NIL)
                  (if stream
                      (eval-stream stream)
                      default)))

; Reads and evaluates each s-expression provided by the specified stream and returns a list containing all evaluation
; results.
(defun eval-stream (stream)
  (let ((evals (list)))
    (loop for expr = (read stream NIL) while expr do
          (setf evals (nconc evals (list (eval expr)))))
    evals))

; Reads and evaluates each s-expression contained in the specified string and returns a list containing all evaluation
; results.
(defun eval-string (string)
  (with-open-stream (stream (make-string-input-stream string))
                    (eval-stream stream)))

; Returns NIL unless there is an item in the specified list denoted by list for which a non-NIL result is returned
; by the specified function denoted by the keyword test (defaults to #'equal) when being passed the respective list
; item and the specified value denoted by val.
(defun in-p (list val &key (test #'equal))
  (and list val (member val list :test test)))

(defun join (stream sep args &optional fn-convert)
  (let (last)
    (dolist (v args)
      (when last (princ sep stream))
      (princ (if fn-convert (funcall fn-convert v) v) stream)
      (setf last v))))

; Returns a string that is a displaced character array, is non-adjustable, starts in val at pos and spans the remainder
; of val unless either end or len are given.
(defun string-displace (val pos len &optional (end NIL))
  (make-array (if end (- end pos) (if len len (- (length val) pos))) :element-type 'character :adjustable NIL :displaced-to val :displaced-index-offset pos))

; Returns NIL unless the val argument is either NIL or the empty string ("").
(defun string-empty-p (val)
  (or (null val) (string= val "")))

; Returns NIL unless the specified string denoted by str ends with the specified string denoted by val.
; If the calling code already computes the (length) of val, pass it for the vlen argument.
; If the calling code already computes the (length) of str, pass it for the slen argument.
(defun string-ends (str val &optional (vlen (length val)) (slen (length str)))
  (and val str (>= slen vlen) (string= str val :start1 (- slen vlen))))

; Returns the zero-based index of the first occurrence in the specified string denoted by str of the specified character
; denoted by val. If val is not a character, it is expected to be a character predicate function.
(defun string-index-of (str val &optional (last NIL))
  (let ((charp (characterp val)) char (len (length str)) (found NIL))
    (dotimes (i len found)
      (setf char (aref str i))
      (when (if charp (char= char val) (funcall val char))
        (if last (setf found i) (return-from string-index-of i))))))

(defun string-join (sep args &optional fn-convert)
  (let (last)
    (with-output-to-string (stream)
                           (join stream sep args fn-convert))))

; Destructively modifies the specified string denoted by val by replacing all its occurrences of the specified character
; or string denoted by old with the character or string denoted by new. If destructive behaviour is acceptable, but not
; strictly required, pass a value other than NIL for the &optional non-destructive-p argument. To ensure destructive
; behaviour in all cases, (setf) the variable passed for the val argument to the resulting value returned by this function.
(defun string-nreplace (val old new &optional (non-destructive-p NIL))
  (let* ((len (length val)) (oldcharp (characterp old)) (newcharp (characterp new)) (lenold (if oldcharp 1 (length old))) (lennew (if newcharp 1 (length new))))
    (if (or (= len 0) (= lenold 0))
        ; val or old are empty: return val
        val
        ; else
        (if (and oldcharp (not newcharp) (= lennew 1))
            (symbol-macrolet ((tmp (string-nreplace val old (aref new 0) non-destructive-p)))
                             (if non-destructive-p tmp (setf val (adjust-array val len :element-type 'character :displaced-to tmp :displaced-index-offset 0))))
            (if (and newcharp (not oldcharp) (= lenold 1))
                (symbol-macrolet ((tmp (string-nreplace val (aref old 0) new non-destructive-p)))
                                 (if non-destructive-p tmp (setf val (adjust-array val len :element-type 'character :displaced-to tmp :displaced-index-offset 0))))
                (if (equal old new)
                    val
                    (if (and (= len 1) (= lenold 1) (char= (aref val 0) (if oldcharp old (aref old 0))))
                        ; val and old are a character or 1-length-string and match the requested replacement: return new
                        (if newcharp (make-string 1 :initial-element new) new)
                        ; else
                        (if (= lenold lennew)
                            ; old and new have the same length, directly manipulate char-array cells of val
                            (let ((start 0))
                              (dotimes (i len val)
                                (setf start (1+ (- i lenold)))
                                (when (and (>= i (1- lenold)) (string= val old :end1 (1+ i) :start1 start))
                                  (dotimes (j lenold)
                                    (setf (aref val (+ start j)) (if newcharp new (aref new j)))))))
                            ; else: old and new have different lengths, temporarily write to a buffering string stream
                            (let* ((skip 0)
                                   ; current char while iterating through val
                                   (char #\x)
                                   ; buffer of the last x chars (length of old)
                                   (buffer (make-array lenold :element-type 'character :adjustable NIL :fill-pointer lenold))
                                   ; temp var to hold resulting new value
                                   (tmp (with-output-to-string (stream)
                                                               ; walk the entire length of val
                                                               (dotimes (k len)
                                                                 (setf char (aref val k))
                                                                 (if (> skip 0)
                                                                     ; store character in temporary buffer
                                                                     (progn
                                                                       (array-shift-append buffer char lenold)
                                                                       (decf skip))
                                                                     ; else
                                                                     (if (< k lenold)
                                                                         ; beginning of val? fill buffer for the first time
                                                                         (setf (aref buffer k) char)
                                                                         ; else
                                                                         (progn
                                                                           (if (string= buffer old)
                                                                               ; if buffer equals old, write new
                                                                               (progn
                                                                                 (if newcharp (write-char new stream) (write-string new stream))
                                                                                 (setf skip (1- lenold)))
                                                                               ; write the char from the buffer we'd otherwise lose in the next iteration
                                                                               (write-char (aref buffer 0) stream))
                                                                           (array-shift-append buffer char lenold)))))
                                                               ; walked through val. now: still chars in the buffer?
                                                               (if (and (= 0 skip) (string= buffer old))
                                                                   ; if buffer equals old, write new
                                                                   (if newcharp (write-char new stream) (write-string new stream))
                                                                   ; else: write the chars in the buffer
                                                                   (write-string buffer stream :start skip)))))
                              ; if destructive should be enforced, set val to new value and return, otherwise just return new value
                              (if non-destructive-p tmp (setf val (adjust-array val (length tmp) :element-type 'character :displaced-to tmp :displaced-index-offset 0))))))))))))

; Destructively modifies the specified string denoted by val by replacing all its occurrences of the specified
; characters or strings denoted by odd-positioned elements in args with the characters or strings denoted by
; even-positioned elements in args. If destructive behaviour is acceptable, but not strictly required, pass a
; value other than NIL for the &optional non-destructive-p argument. To ensure destructive behaviour in all cases,
; (setf) the variable passed for the val argument to the resulting value returned by this function.
(defun string-nreplace* (val args &optional (non-destructive-p NIL))
  (let* ((old (car args))
         (new (cadr args))
         (rest-list (cddr args))
         (result (if (not rest-list)
                     (string-nreplace val old new non-destructive-p)
                     (string-nreplace (string-nreplace* val rest-list) old new non-destructive-p))))
    (if non-destructive-p result (setf val result))))

; Replaces each occurrence of more than max-occurs concsecutive char characters in val with exactly max-occurs
; consecutive char characters. If destructive behaviour is acceptable, but not strictly required, pass a
; value other than NIL for the &optional non-destructive-p argument. To ensure destructive behaviour in all cases,
; (setf) the variable passed for the val argument to the resulting value returned by this function.
(defun string-nreplace-reduce (val char max-occurs &optional (non-destructive-p NIL))
  (let* ((offender (make-string (1+ max-occurs) :initial-element char))
         (remedy (make-string max-occurs :initial-element char))
         (result (if non-destructive-p
                     (string-nreplace val offender remedy non-destructive-p)
                     (setf val (string-nreplace val offender remedy non-destructive-p))))
         (pos (search offender result :test #'equal)))
    (if (not pos)
        (if non-destructive-p
            result
            (setf val result))
        (if non-destructive-p
            (string-nreplace-reduce result char max-occurs non-destructive-p)
            (setf val (string-nreplace-reduce result char max-occurs non-destructive-p))))))

; Returns a string representing the specified string denoted by val, in which all occurrences of the specified string
; denoted by old have been replaced with the specified string denoted by new.
(defun string-replace (val old new)
  (string-nreplace (copy val) old new T))

; Returns a string representing the specified string denoted by val, in which all its occurrences of the specified
; characters or strings denoted by odd-positioned elements in args have been replaced with the characters or strings
; denoted by even-positioned elements in args.
(defun string-replace* (val args)
  (string-nreplace* (copy val) args T))

; Replaces each occurrence of more than max-occurs concsecutive char characters in val with exactly max-occurs
; consecutive char characters.
(defun string-replace-reduce (val char max-occurs)
  (string-nreplace-reduce (copy val) char max-occurs T))

; Returns NIL unless the specified string denoted by str starts with the specified string denoted by val.
; If the calling code already computes the (length) of val, pass it for the vlen argument.
; If the calling code already computes the (length) of str, pass it for the slen argument.
(defun string-starts (str val &optional (vlen (length val)) (slen (length str)))
  (and val str (>= slen vlen) (string= str val :end1 vlen)))

(defun string-sub (str start end &key (start-last NIL) (end-last NIL))
  (if str
      (let* ((index (if start (string-index-of str start start-last) NIL)) (sub (if index (subseq str (1+ index)) str)))
        (if (and end sub) (subseq sub 0 (string-index-of sub end end-last)) sub))
      NIL))

; defining the reader macro to translate s-expressions within the #[ and ] delimiter characters into a (gethash) call.
(defdelim #\[ #\] #'(lambda (list)
                      `(gethash ,(cadr list) ,(car list) ,(caddr list))))

; defining the reader macro to translate s-expressions within the #{ and } delimiter characters into a (aref) call.
(defdelim #\{ #\} #'(lambda (list)
                      `(aref ,(car list) ,(cadr list))))
