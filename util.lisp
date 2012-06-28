(in-package :info.read-eval-print.active-record)

(defgeneric pluralize (x)
  (:method (x)
    (pluralize (string-downcase x))))

(defmethod pluralize ((string string))
  (let* (flush add
               (len (length string))
               (last-char-raw (char string (1- len)))
               (last-char (char-upcase last-char-raw))
               (penult-char (char-upcase (if (> len 1)
                                             (char string (- len 2))
                                             #\Nul))) ;dummy
               (last-3 (subseq string (max 0 (- len 3)))))
    (declare (character last-char-raw last-char penult-char)
             (string last-3))
    (setf (values flush add)
          (cond ((and (char-equal last-char #\Y)
                      (not (member penult-char '(#\A #\E #\I #\O #\U))))
                 (values 1 "ies"))
                ((or (string-equal string "ox")
                     (string-equal string "vax"))
                 (values nil "en"))
                ((or (and (char= last-char #\H)
                          (member penult-char '(#\C #\S)))
                     (member last-char '(#\S #\Z #\X)))
                 (values nil "es"))
                ((string-equal last-3 "man")
                 (values 2 "en"))
                ((string-equal last-3 "ife")
                 (values  2 "ves"))
                (t (values nil "s"))))
    (when flush
      (setq string (subseq string 0 (- len flush))))
    (concatenate 'string string add)))

(defgeneric singularize (x)
  (:method (x)
    (singularize (string-downcase x)))
  (:method ((string string))
    (cond ((alexandria:ends-with-subseq "ies" string)
           (ppcre:regex-replace "ies$" string "y"))
          ((alexandria:ends-with-subseq "IES" string)
           (ppcre:regex-replace "IES$" string "Y"))
          ((or (alexandria:ends-with #\s string)
               (alexandria:ends-with #\S string))
           (subseq string 0 (1- (length string))))
          (t
           string))))

(defun sym (&rest args)
  (intern
   (with-output-to-string (out)
     (loop for x in args
           do (write-string (string-upcase x) out)))
   *package*))

(defun key-sym (&rest args)
  (let ((*package* (find-package :keyword)))
    (apply #'sym args)))

(defun str (&rest args)
  (format nil "~{~a~}" args))

(defmacro with-keywords-removed ((options &rest keywords) &body body)
  `(let ((,options (loop for (k v) on ,options by #'cddr
                         unless (member k ',keywords :test #'eq)
                           append (list k v))))
     ,@body))

(defgeneric to-class (thing)
  (:method ((thing string))
    (to-lisp-token (singularize thing)))
  (:method ((thing symbol))
    (to-class (symbol-name thing))))

(defgeneric to-lisp-token (thing &optional package)
  (:method ((string string) &optional (package *package*))
    (intern (substitute #\- #\_ (if (some #'upper-case-p string)
                                    string
                                    (string-upcase string)))
            package)))

(defgeneric to-keyword (string)
  (:method ((string string))
    (to-lisp-token string :keyword))
  (:method ((x symbol))
    (if (keywordp x)
        x
        (intern (symbol-name x) :keyword))))

(defgeneric to-sql-token (thing)
  (:method (thing)
    (format nil "~a" thing))
  (:method ((thing symbol))
    (let ((name (symbol-name thing)))
      (substitute #\_ #\-
                  (if (some #'lower-case-p name)
                      name
                      (string-downcase name))))))

(defgeneric to-column-name (thing)
  (:method ((x string))
    x)
  (:method ((x symbol))
    (to-sql-token x)))

(defgeneric to-column-symbol (thing &optional package)
  (:method ((x symbol) &optional package)
    (declare (ignore package))
    x)
  (:method ((x string) &optional (package *package*))
    (intern x package)))

(defgeneric to-table-name (thing)
  (:method ((x string))
    x)
  (:method ((x symbol))
    (pluralize (to-sql-token x))))

(defgeneric to-table-symbol (thing)
  (:method ((x symbol))
    x)
  (:method ((x string))
    (singularize (to-lisp-token x))))

(defgeneric sanitize-sql (x)
  (:method ((x string))
    (format nil "'~a'" (ppcre:regex-replace-all "'" x "''")))
  (:method ((x list))
    (format nil "(~{~a~^, ~})" (mapcar #'sanitize-sql x)))
  (:method (x)
    (princ-to-string x)))

(defgeneric to-sql-value (thing)
  (:method (x)
    (sanitize-sql x)))

(defun to-camel (string &key (first-upcase t))
  (with-output-to-string (out)
    (let ((word (scan-split "[_-]" (string string)))
          (fun (latch (series (if first-upcase #'string-capitalize #'string-downcase))
                      :after 1 :post #'string-capitalize)))
      (collect-ignore
       (write-string (funcall fun word) out)))))

(defun to-snake (string)
  (format nil "~{~(~a~)~^-~}" (ppcre:all-matches-as-strings ".[^A-Z]*" string)))
