(in-package :info.read-eval-print.active-record)

(defvar *connection* nil "database connection")
(defvar *association* nil "association")

(defvar *connection-spec* '("localhost" "outing_development" "root" ""))
(setq clsql-sys:*default-database-type* :mysql)

(defgeneric after-connect (database-type)
  (:method ((database-type (eql :mysql)))
    (clsql-sys:execute-command "set names utf8")))

(defun establish-connection (&optional (connection-spec *connection-spec*)
                               (database-type clsql-sys:*default-database-type*))
  (clsql-sys:connect connection-spec
                     :database-type database-type
                     :if-exists :old)
  (after-connect database-type))

(defgeneric to-sql-token (thing)
  (:method (thing)
    (format nil "~a" thing))
  (:method ((thing symbol))
    (let ((name (symbol-name thing)))
      (if (some #'lower-case-p name)
          name
          (string-downcase name)))))

(defgeneric to-keyword (string)
  (:method ((string string))
    (intern (if (some #'upper-case-p string)
                string
                (substitute #\- #\_ (string-upcase string)))
            :keyword)))

(defun sql->sym (name &key (package *package* packagep) (downcase nil))
  (flet ((normalize-case (str)
           (funcall (if downcase #'string-downcase #'string-upcase)
                    str))
         (subst-hyphen (x)
           (substitute #\- #\_ x)))
    (intern (subst-hyphen (normalize-case (string name)))
            (if packagep package *package*))))

(defclass* column ()
  ((key)
   (name)
   (type)
   (precision nil)
   (scale nil)
   (nullable nil :accessor nullable-p)
   (default nil)))

(defclass* active-record-class (standard-class)
  ((table-name)
   (columns nil)
   (associations nil)))

(defmethod print-object ((class active-record-class) stream)
  (print-unreadable-object (class stream :type t)
    (format stream "~a ~{~a~^ ~}" (class-name class)
            (mapcar #'name-of (columns-of class)))))

(defmethod c2mop:validate-superclass ((class active-record-class) (super standard-class))
  t)

(defmethod c2mop:validate-superclass ((class standard-class)
                                      (super active-record-class))
  nil)




(defclass ar-slot-mixin ()
  ((column :accessor slot-definition-column
           :initarg :column
           :initform nil)))

(defclass ar-direct-slot-definition
    (ar-slot-mixin c2mop:standard-direct-slot-definition)
  ())

(defclass ar-effective-slot-definition
    (ar-slot-mixin c2mop:standard-effective-slot-definition)
  ())

(defclass ar-belongs-to-slot-mixin ()
  ((belongs-to :initarg :belongs-to
               :initform nil
               :accessor belongs-to)
   (foreign-slotname :initarg :foreign-slotname
                     :initform nil
                     :accessor foreign-slotname)))

(defclass ar-belongs-to-direct-slot-definition (ar-direct-slot-definition
                                                ar-belongs-to-slot-mixin)
  ())

(defclass ar-belongs-to-effective-slot-definition (ar-effective-slot-definition
                                                   ar-belongs-to-slot-mixin)
  ())

(defmacro def-has-xxx-slot-definition (xxx
                                       default-class-symbol-form)
  `(progn
     (defclass ,(sym "ar-has-" xxx "-slot-mixin") ()
       ((,(sym "has-" xxx) :initarg ,(key-sym "has-" xxx)
                           :initform nil
                           :accessor ,(sym "has-" xxx))
        (class-symbol :initarg :class-symbol
                      :initform nil
                      :accessor class-symbol)))

     (defmethod initialize-instance :after ((self ,(sym "ar-has-" xxx
                                                        "-slot-mixin"))
                                            &rest args)
       (declare (ignore args))
       (unless (class-symbol self)
         (setf (class-symbol self) ,default-class-symbol-form)))

     (defclass ,(sym "ar-has-" xxx "-direct-slot-definition")
         (ar-direct-slot-definition ,(sym "ar-has-" xxx "-slot-mixin"))
       ())

     (defclass ,(sym "ar-has-" xxx "-effective-slot-definition")
         (ar-effective-slot-definition ,(sym "ar-has-" xxx "-slot-mixin"))
       ())
     ))

(def-has-xxx-slot-definition one (has-one self))
(def-has-xxx-slot-definition many (sym (singularize (has-many self))))

(defmethod c2mop:direct-slot-definition-class ((class active-record-class)
                                               &rest initargs)
  (find-class
   (cond ((getf initargs :belongs-to)
          'ar-belongs-to-direct-slot-definition)
         ((getf initargs :has-many)
          'ar-has-many-direct-slot-definition)
         ((getf initargs :has-one)
          'ar-has-one-direct-slot-definition)
         (t 'ar-direct-slot-definition))))

;; compute-effective-slot-definition-initargs がポータブルではないので
(defvar *effective-slot-definition-class* nil)

(defmethod c2mop:effective-slot-definition-class ((class active-record-class)
                                                  &rest initargs)
  (declare (ignore initargs))
  (find-class
   (or *effective-slot-definition-class*
       'ar-effective-slot-definition)))

(defmethod c2mop:compute-effective-slot-definition
    ((class active-record-class)
     slot-name
     direct-slot-definitions)
  (let ((dslotd (car direct-slot-definitions)))
    (typecase dslotd
      (ar-belongs-to-direct-slot-definition
       (let* ((*effective-slot-definition-class*
                'ar-belongs-to-effective-slot-definition)
              (esd (call-next-method)))
         (setf (belongs-to esd) (belongs-to dslotd))
         (setf (foreign-slotname esd) (foreign-slotname dslotd))
         esd))
      (ar-has-many-direct-slot-definition
       (let* ((*effective-slot-definition-class*
                'ar-has-many-effective-slot-definition)
              (esd (call-next-method)))
         (setf (has-many esd) (has-many dslotd)
               (class-symbol esd) (class-symbol dslotd))
         esd))
      (ar-has-one-direct-slot-definition
       (let* ((*effective-slot-definition-class*
                'ar-has-one-effective-slot-definition)
              (esd (call-next-method)))
         (setf (has-one esd) (has-one dslotd)
               (class-symbol esd) (class-symbol dslotd))
         esd))
      (t (setf *effective-slot-definition-class* nil)
       (call-next-method)))))

(defmethod c2mop:slot-value-using-class
    ((class active-record-class)
     instance
     (slot-def ar-belongs-to-effective-slot-definition))
  (aif (call-next-method)
       it
       (setf (slot-value instance (belongs-to slot-def))
             (select (find-class (belongs-to slot-def))
                     (slot-value instance (foreign-slotname slot-def))))))

(defmethod (setf c2mop:slot-value-using-class) :after
  (new-value
   (class active-record-class)
   instance
   (slot-def ar-belongs-to-effective-slot-definition))
  (setf (slot-value instance (foreign-slotname slot-def))
        (and new-value (value-of new-value :id))))

(defmethod c2mop:slot-value-using-class
    ((class active-record-class)
     instance
     (slot-def ar-has-many-effective-slot-definition))
  (aif (call-next-method)
       it
       (setf (slot-value instance (has-many slot-def))
             (all (find-class (class-symbol slot-def))
                  :conditions (list (key-sym (class-name class) '-id)
                                    (value-of instance :id))))))

(defmethod (setf c2mop:slot-value-using-class) :after
  (new-value
   (class active-record-class)
   instance
   (slot-def ar-has-many-effective-slot-definition))
  (loop with id = (value-of instance :id)
        with column = (str (class-name class) "-id")
        for x in new-value
        do (setf (value-of x column) id)))

(defmethod c2mop:slot-value-using-class
    ((class active-record-class)
     instance
     (slot-def ar-has-one-effective-slot-definition))
  (aif (call-next-method)
       it
       (setf (slot-value instance (has-one slot-def))
             (car (all (find-class (class-symbol slot-def))
                       :conditions (list (key-sym (class-name class) '-id)
                                         (value-of instance :id)))))))

(defmethod (setf c2mop:slot-value-using-class) :after
  (new-value
   (class active-record-class)
   instance
   (slot-def ar-has-one-effective-slot-definition))
  (when new-value
    (setf (value-of new-value (str (class-name class) "-id"))
          (value-of instance :id))))


(define-method-combination active-record ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  "before メソッドが nil を返した場合メソッドの実行を中断する。"
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods))
         (call-methods-and (methods)
           `(and ,@(mapcar #'(lambda (method)
                               `(call-method ,method))
                           methods))))
    (let ((form (if (or before after (rest primary))
                    `(when ,(call-methods-and before)
                       (multiple-value-prog1
                           (call-method ,(first primary)
                                        ,(rest primary))
                         ,@(call-methods (reverse after))))
                    `(call-method ,(first primary)))))
      (if around
          `(call-method ,(first around)
                        (,@(rest around)
                         (make-method ,form)))
          form))))

(defun where (&rest args)
  (apply #'%where *connection* *association* args))

(defun as-list ()
  (%as-list *connection* *association*))

(defgeneric %where (connection association &rest args))
(defgeneric %as-list (connection association))

(defmethod %where (connection association &rest args)
  (push args (slot-value association 'wheres)))

(defmacro with-association ((table) &body body)
  `(let ((*association* (ensure-association ,table)))
     ,@body))

(defmethod %to-sql (connection association)
  (with-slots (selects joins wheres orders groups) association
    (with-slots (table-name) (base-of association)
      (with-output-to-string (out)
        (write-string "select" out)
        (if selects
            (collect-ignore
             (progn
               (write-char #\space out)
               (write-string (to-sql-token (scan-lists-of-lists selects)) out)))
            (format out " ~a.*" table-name))
        (format out " from ~a" table-name)))))

(defmethod %as-list (connection association)
  (clsql:query (%to-sql connection association)))

(defclass* association ()
  ((base)
   (selects nil)
   (joins   nil)
   (wheres  nil)
   (orders  nil)
   (groups  nil)
   (limit   nil)
   (offset  nil)))

(defparameter base
  (defclass base ()
    ((new-record :initarg :new-record :initform t :accessor new-record-p))
    (:metaclass active-record-class)))

(defmethod ensure-association ((association association))
  association)

(defmethod ensure-association (base)
  (make-instance 'association :base base))

(defmethod table-name-of ((x base))
  (table-name-of (class-of x)))

(defmethod columns-of ((x base))
  (columns-of (class-of x)))

(defgeneric value-of (record column)
  (:method (record (column symbol))
    (value-of record (symbol-name column)))
  (:method (record (column string))
    (value-of record (find-column (class-of record) column)))
  (:method (record (column column))
    (slot-value record (name-of column))))

(defgeneric (setf value-of) (value record column)
  (:method (value record (column symbol))
    (setf (value-of record (symbol-name column)) value))
  (:method (value record (column string))
    (setf (value-of record (find-column (class-of record) column)) value))
  (:method (value record (column column))
    (setf (slot-value record (name-of column)) value)))

(defgeneric coerce-sql-symbol (x)
  (:method ((x string))
    x)
  (:method ((x null))
    "null")
  (:method ((x symbol))
    (substitute #\_ #\- (string-downcase (symbol-name x))))
  (:method ((x clsql-sys:wall-time))
    (clsql-sys::db-timestring x))
  (:method (x)
    x))

(defgeneric coerce-sql-value (x)
  (:method ((x string))
    (with-output-to-string (out)
      (write-char #\' out)
      (map nil (lambda (x)
                 (if (char= x #\')
                     (write-string "''" out)
                     (write-char x out)))
           x)
      (write-char #\' out)))
  (:method ((x cons))
    (format nil "(~{~a~^, ~})" (mapcar #'coerce-sql-value x)))
  (:method ((x null))
    "null")
  (:method ((x symbol))
    (substitute #\_ #\- (string-downcase (symbol-name x))))
  (:method ((x clsql-sys:wall-time))
    (clsql-sys::db-timestring x))
  (:method (x)
    x))

(defmethod make-instance-from-row (class row fields)
  (let ((instance (make-instance class)))
    (loop for col in fields
          for i from 0
          for x = (find-column class col)
          if x
            do (setf (value-of instance x) (nth i row)))
    (setf (new-record-p instance) nil)
    instance))

(defgeneric find-column (active-record-class x)
  (:method ((class active-record-class) (x symbol))
    (find-column class (symbol-name x)))
  (:method ((class active-record-class) (x string))
    (find (substitute #\_ #\- (string-downcase x))
          (columns-of class) :key #'name-of
                             :test #'string=)))

(defgeneric columns-expect-id (active-record-class)
  (:method ((class active-record-class))
    (loop for x in (columns-of class)
          unless (string= "id" (name-of x))
            collect x)))


(defgeneric save (record)
  (:method-combination active-record)
  (:method ((self base))
    (create-or-update self)))

(defgeneric create-or-update (record)
  (:method-combination active-record)
  (:method ((self base))
    (if (new-record-p self)
        (create self)
        (update self))))

(defmethod save :before (record)
  (let* ((class (class-of record))
         (created-at (find-column class :created-at))
         (updated-at (find-column class :updated-at))
         (time (clsql-sys::get-time)))
    (when (and created-at (null (value-of record created-at)))
      (setf (value-of record created-at) time))
    (when updated-at
      (setf (value-of record updated-at) time))
    t))

(defgeneric create (record)
  (:method-combination active-record)
  (:method ((self base))
    (let ((slots (mapcar #'name-of (columns-expect-id (class-of self)))))
      (clsql-sys:execute-command
       (format nil "insert into ~a (~{~a~^,~}) values (~{~a~^,~})"
               (table-name-of self)
               (mapcar #'coerce-sql-symbol slots)
               (mapcar (lambda (x) (coerce-sql-value (slot-value self x)))
                       slots)))
      (setf (new-record-p self) t
            (value-of self :id)
            (caar (clsql-sys:query "select last_insert_id()"))))
    self))

(defgeneric update (record)
  (:method-combination active-record)
  (:method ((self base))
    (let ((slots (mapcar #'name-of (columns-expect-id (class-of self)))))
      (clsql-sys:execute-command
       (format nil "update ~a set ~{~a = ~a~^,~} where id = ~a"
               (table-name-of self)
               (loop for x in slots
                     append (list (coerce-sql-symbol x)
                                  (coerce-sql-value (slot-value self x))))
               (value-of self :id))))
    self))

(defgeneric destroy (record)
  (:method-combination active-record)
  (:method ((self base))
    (clsql-sys:execute-command (format nil "delete from ~a where id = ~d"
                                       (table-name-of (class-of self))
                                       (value-of self :id)))))

(defvar *class-options* (make-hash-table))

(defmethod activi-record-class-option-form ((option (eql :has-many)) class table-name &rest option-args)
  (let ((children (car option-args)))
    (values
     ;; slots
     `((,children :initform nil))
     ;; methods
     `((defmethod ,(sym children '-of) ((self ,class))
         (aif (slot-value self ',children)
              it
              select))))))

(setf (gethash :has-many *class-options*) 'activi-record-class-option-form)

(defmacro def-record (name &rest options)
  (let* ((table-name (substitute #\_ #\- (pluralize name)))
         (attributes (clsql-sys:list-attribute-types table-name))
         (columns (loop for (column-name type precision scale nullable)
                        in attributes
                        collect (make-instance
                                 'column
                                 :name (sql->sym column-name :package *package*)
                                 :name-string column-name
                                 :key (sql->sym column-name :package :keyword)
                                 :type type
                                 :precision precision
                                 :scale scale
                                 :nullable nullable))))
    `(progn
       (defparameter ,name
         (defclass ,name (base)
           (,@(loop for x in columns
                    collect (list (name-of x)
                                  :initarg (key-of x)
                                  :initform nil
                                  :accessor (sym (name-of x) '-of)))
            ,@(loop for (association table) in options
                    ;; belongs-to
                    if (eq :belongs-to association)
                      collect (list table
                                    :initform nil
                                    :belongs-to table
                                    :foreign-slotname (sym table '-id)
                                    :accessor (sym table '-of))
                    ;; has-many
                    if (eq :has-many association)
                      collect (list table
                                    :initform nil
                                    :has-many table
                                    :class-symbol (sym (singularize table))
                                    :accessor (sym table '-of))
                    ;; has-one
                    if (eq :has-one association)
                      collect (list table
                                    :initform nil
                                    :has-one table
                                    :class-symbol table
                                    :accessor (sym table '-of))
                    ))
           (:metaclass active-record-class)))

       (setf (table-name-of ,name) ,table-name)
       (setf (columns-of ,name)
             (loop for (column-name type precision scale nullable)
                   in ',attributes
                   collect (make-instance
                            'column
                            :name (sql->sym column-name :package *package*)
                            :name-string column-name
                            :key (sql->sym column-name :package :keyword)
                            :type type
                            :precision precision
                            :scale scale
                            :nullable nullable)))

       (defmethod print-object ((self ,name) stream)
         (print-unreadable-object (self stream :type t :identity t)
           (format stream "~@{~a: ~s~^, ~}"
                   ,@(loop for x in columns
                           append (list `',(name-of x)
                                        `(,(sym (name-of x)'-of) self)))))))))

(defun make-column (name type precision scale nullable)
  (make-instance 'column
                 :key (to-keyword name)
                 :name name
                 :type type
                 :precision precision
                 :scale scale
                 :nullable nullable))

(defun column-to-slot-definition (column)
  (let ((name (intern (symbol-name (key-of column)) *package*)))
    `(,name :initarg ,(key-of column)
            :initform (and (not (nullable-p column))
                            (default-of column))
            :accessor ,(intern (concatenate 'string (symbol-name name) "-OF") *package*))))

(defmacro defrecord (class super-classes slots &rest options)
  (let* ((table-name (or (cadr (assoc :table-name options))
                         (let ((table-name (pluralize (to-sql-token class))))
                           (push `(:table-name ,table-name) options)
                           table-name)))
         (columns (mapcar (lambda (x) (apply #'make-column x))
                          (clsql-sys:list-attribute-types table-name))))
    `(progn

       (defparameter ,class
         (defclass ,class ,(or super-classes '(base))
           (,@slots
            ,@(mapcar #'column-to-slot-definition columns))
           (:metaclass active-record-class)))

       (setf (table-name-of ,class) ,table-name)
       (setf (columns-of ,class) (list ,@columns))

       ,class)))


(defclass has-many-slot-mixin ()
  ())
(defclass has-many-direct-slot-definition (ar-direct-slot-definition has-many-slot-mixin)
  ())

(defmethod c2mop:slot-value-using-class (class instance (slot many-effective-slot-definition))
  (let ((slot-name (c2mop:slot-definition-name slot)))
    (if (slot-boundp instance slot-name)
        (slot-value instance slot-name)
        (setf (slot-value instance slot-name)
              (fetch-association class instance slot)))))


#|
(establish-connection)

(defrecord facility ()
  ())

(with-association (facility)
  (where :name "foo")
  (as-list))


(def-record facility ()
  ()
  (:has-many experiences))

(with-association (facility)
  (joins :experiences)
  (where :publish t
          :prefecture 13
          `(:experiences :publish) t)
  (as-list))

(def-record experience ()
  ()
  (:belongs-to facility))
|#
