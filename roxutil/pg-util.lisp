(in-package :roxutil)

(defmacro do-for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro do-until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro do-while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

; Binds the specified symbol denoted by var to the result of evaluating the expression denoted by val and
; evaluates the form denoted by then unless var is now NIL, in which case, the form denoted by else is evaluated.
(defmacro if-let (var val then else)
  `(let ((,var ,val))
     (if ,var ,then ,else)))

; Binds the specified symbol denoted by var to the result of evaluating the expression denoted by val and
; evaluates the forms denoted by body unless var is now NIL.
(defmacro when-let (var val &body body)
  `(let ((,var ,val))
     (when ,var
       ,@body)))

; Returns the specified value denoted by val if it is a list; otherwise, returns a new list containing only val.
(defun 2list (val)
  (if (listp val) val (list val)))

; Returns non-NIL if the specified value denoted by one "occurs" (as tested by the specified function denoted by test)
; after the specified value denoted by two in the specified list.
(defun after (list one two &key (test #'eql))
  (let ((rest (before list two one :test test)))
    (and rest (member one rest :test test))))

; Returns a new list representing the specified list with the specified value denoted by val appended to it.
(defun append1 (list val)
  (append list (list val)))

; Returns non-NIL if the specified value denoted by one "occurs" (as tested by the specified function denoted by test)
; before the specified value denoted by two in the specified list, or if one does but two doesn't.
(defun before (list one two &key (test #'eql))
  (and list
       (let ((first (car list)))
         (cond ((funcall test two first) NIL)
               ((funcall test one first) list)
               (T (before (cdr list) one two :test test))))))

; Returns the item in the specified list that "beats" (as defined by the specified function denoted by fn) all others.
(defun best (list fn)
  (if (null list)
      NIL
      (let ((winner (car list)))
        (dolist (val (cdr list))
          (when (funcall fn val winner)
            (setq winner val)))
        winner)))

; Returns non-NIL if the specified value denoted by val "occurs" (as tested by the specified function denoted by
; test) more than once in the specified list.
(defun duplicate (list val &key (test #'eql))
  (member val (cdr (member val list :test test)) :test test))

; Returns a list of non-NIL values returned by the specified function denoted by fn when called with the items
; contained in the specified list. 
(defun filter (fn list)
  (let ((acc NIL))
    (dolist (v list)
      (when-let val (funcall fn v) (push val acc)))
    (nreverse acc)))

; Returns both the first item in the specified list for which the specified function denoted by fn returns a non-NIL
; value when applied to the item, as well as the value returned by fn.
(defun find1 (list fn)
  (if (null list)
      NIL
      (let* ((car (car list)) (val (funcall fn car)))
        (if val
            (values car val)
            (find1 (cdr list) fn)))))

; Returns a list of all atoms in the specified list or any sub-lists. 
(defun flatten (list &optional (acc NIL))
  (cond ((null list) acc)
        ((atom list) (cons list acc))
        (T (flatten (car list) (flatten (cdr list) acc)))))

; Returns a new list of lists containing the items of the specified list, each guaranteed to be no longer than the
; specified maximum length denoted by len, which must be greater than zero. 
(defun group (list len)
  (when (zerop len)
    (error "(group) called with len of 0"))
  (labels ((rec (list acc)
             (let ((rest (nthcdr len list)))
               (if (consp rest)
                   (rec rest (cons (subseq list 0 len) acc))
                   (nreverse (cons list acc))))))
    (if list (rec list NIL) NIL)))

; Returns the last item in the specified list.
(defun last1 (list)
  (car (last list)))

; Determines whether the specified denoted by list1 is longer than the specified list denoted by list2, without
; computing the full (length)s of both lists.
(defun longer (list1 list2)
  (labels ((compare (one two)
             (and (consp one) (or (null two) (compare (cdr one) (cdr two))))))
    (if (and (listp list1) (listp list2))
        (compare list1 list2)
        (> (length list1) (length list2)))))

; Applies the specified function denoted by fn to all values ranging from a to b, and returns a list containing all
; the results returned by fn. The specified step function denoted by fn-step (defaulting to #'+) determines how to
; loop through the desired range. If the specified step is non-NIL, fn-step is applied to the current value and step,
; otherwise it is applied to the current value only. The specified test function denoted by fn-test (defaulting to #'>)
; determines when to stop mapping. If the specified end of the range denoted by b is non-NIL, fn-test is applied to the
; current value and b; otherwise, it is applied to the current value only.
(defun map-> (fn a b &key (step (if (and (numberp b) (numberp a)) (if (< b a) -1 1) NIL)) (fn-step #'+) (fn-test #'>))
  (do ((i a (if step (funcall fn-step i step) (funcall fn-step i)))
       (result nil))
      ((if b (funcall fn-test i b) (funcall fn-test i)) (nreverse result))
    (push (funcall fn i) result)))

; Applies the specified function denoted by fn to all values ranging from 0 to n, and returns a list containing all
; the results returned by fn.
(defun map0-n (fn n)
  (map-> fn 0 n))

; Applies the specified function denoted by fn to all values ranging from 1 to n, and returns a list containing all
; the results returned by fn.
(defun map1-n (fn n)
  (map-> fn 1 n))

; Returns a new result list of applying #'mapcar to the specified lists.
(defun mappend (fn-map &rest lists)
  (apply #'append (apply #'mapcar fn-map lists)))

; Returns a function that when called caches the result of invoking the specified function denoted by fn
; and returns the cached result upon future invokations with an #'equal set of arguments.
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (return-value found-p) (gethash args cache)
          (if found-p
              return-value
              (setf (gethash args cache) (apply fn args)))))))

; Applying the specified scoring function denoted by fn to each item in the specified list, returns both the first
; item that scored highest compared to all other items as well as its score.
(defun most (list fn)
  (if (null list)
      (values NIL NIL)
      (let* ((wins (car list)) (max (funcall fn wins)))
        (dolist (val (cdr list))
          (let ((score (funcall fn val)))
            (when (> score max)
              (setq wins val max score))))
        (values wins max))))

; Applying the specified scoring function denoted by fn to each item in the specified list, returns both a list of
; all items that scored highest compared to other items, as well as their score.
(defun most* (list fn)
  (if (null list)
      (values nil nil)
      (let* ((car (car list)) (result (list car)) (max (funcall fn car)))
        (dolist (val (cdr list))
          (let ((score (funcall fn val)))
            (cond ((> score max)
                   (setq max score result (list val)))
                  ((= score max)
                   (push val result)))))
        (values (nreverse result) max))))

; Destructively modifies the specified list by appending to it the specified value denoted by val.
(defun nappend1 (list val)
  (nconc list (list val)))

; Returns a new list representing the specified list denoted by tree, with all its atoms and its sub-lists' atoms
; removed if they cause the specified function denoted by test to return NIL when applied to it.
(defun prune (tree test &optional (acc NIL))
  (let ((car (car tree)) (cdr (cdr tree)))
    (cond ((null tree) (nreverse acc))
          ((consp car) (prune cdr test (cons (prune car test NIL) acc)))
          (T (prune cdr test (if (funcall test car) acc (cons car acc)))))))

; Returns NIL unless the specified list contains exactly one item, which is otherwise returned.
(defun single1 (list)
  (if (cdr list) NIL (if (consp list) (car list) NIL)))

; Returns two lists; the first list containing all items in the specified list before the one which first
; caused the specified function denoted by fn to return a non-NIL value when applied to it, the second list containing
; said item and all following items.
(defun split-at (list fn)
  (let ((acc nil))
    (do ((src list (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))
