(in-package :info.read-eval-print.active-record)

(defvar *connection* nil "database connection")
(defvar *association* nil "association")

(defvar *connection-spec* '("localhost" "outing_development" "root" ""))
(setq clsql-sys:*default-database-type* :mysql)

(defgeneric after-connect (database-type)
  (:method ((database-type (eql :mysql)))
    (clsql-sys:start-sql-recording)
    (clsql-sys:execute-command "set names utf8")))

(defun establish-connection (&optional (connection-spec *connection-spec*)
                               (database-type clsql-sys:*default-database-type*))
  (clsql-sys:connect connection-spec
                     :database-type database-type
                     :if-exists :old)
  (after-connect database-type))


(defclass* active-record-class (standard-class)
  ((table-name)))

(defmethod print-object ((class active-record-class) stream)
  (print-unreadable-object (class stream :type t)
    (format stream "~a ~{~a~^ ~}" (class-name class)
            (map-column-slot #'c2mop:slot-definition-name class))))

(defun find-slot-by-%key (class %key)
  (find %key (c2mop:class-slots class) :key #'%key-of :test #'eq))

(defmethod c2mop:validate-superclass ((class active-record-class) (super standard-class))
  t)

(defmethod c2mop:validate-superclass ((class standard-class)
                                      (super active-record-class))
  nil)


(defclass* ar-slot-mixin ()
  ((%key nil)
   (column-name nil)
   (sql-type nil)
   (precision nil)
   (scale nil)
   (nullable nil :accessor nullable-p)
   (default nil)))

(defclass ar-direct-slot-definition
    (ar-slot-mixin c2mop:standard-direct-slot-definition)
  ())

(defclass ar-effective-slot-definition
    (ar-slot-mixin c2mop:standard-effective-slot-definition)
  ())

(defclass column-direct-slot-definition (ar-direct-slot-definition)
  ())

(defclass  column-effective-slot-definition (ar-effective-slot-definition)
  ())


(defun map-column-slot (function class)
  (let ((slot (scan (c2mop:class-slots class))))
    (collect (choose (typep slot 'column-effective-slot-definition)
                     (funcall function slot)))))

(defun scan-column-slot (class)
  (declare (optimizable-series-function))
  (choose-if (lambda (x)
               (typep x 'column-effective-slot-definition))
             (scan 'list (c2mop:class-slots class))))

(defclass* has-many-slot-mixin ()
  ((has-many)
   (association-class)
   (dependent)
   (as)
   (foreign-key)
   (foreign-type)
   (primary-key)
   (order)))

(defclass has-many-direct-slot-definition (ar-direct-slot-definition has-many-slot-mixin)
  ())

(defclass has-many-effective-slot-definition (ar-effective-slot-definition has-many-slot-mixin)
  ())

(defclass* belongs-to-slot-mixin ()
  ((belongs-to)
   (association-class)
   (polymorphic)
   (foreign-key)
   (foreign-type)
   (primary-key)
   (dependent)))

(defclass belongs-to-direct-slot-definition (ar-direct-slot-definition belongs-to-slot-mixin)
  ())

(defclass belongs-to-effective-slot-definition (ar-effective-slot-definition belongs-to-slot-mixin)
  ())

(defmethod c2mop:direct-slot-definition-class ((class active-record-class)
                                               &rest initargs)
  (find-class
   (cond ((getf initargs :column-name)
          'column-direct-slot-definition)
         ((getf initargs :belongs-to)
          'belongs-to-direct-slot-definition)
         ((getf initargs :has-many)
          'has-many-direct-slot-definition)
         ((getf initargs :has-one)
          'has-one-direct-slot-definition)
         (t 'ar-direct-slot-definition))))

;; compute-effective-slot-definition-initargs がポータブルではないので
(defvar *effective-slot-definition-class* nil)

(defmethod c2mop:effective-slot-definition-class ((class active-record-class)
                                                  &rest initargs)
  (declare (ignore initargs))
  (find-class (or *effective-slot-definition-class*
                  'c2mop:standard-effective-slot-definition)))

(defmethod c2mop:compute-effective-slot-definition
    ((class active-record-class)
     slot-name
     direct-slot-definitions)
  (let ((dslotd (car direct-slot-definitions)))
    (typecase dslotd
      (column-direct-slot-definition
       (let* ((*effective-slot-definition-class* 'column-effective-slot-definition)
              (esd (call-next-method)))
         (dolist (slot (c2mop:class-slots (class-of dslotd)))
           (let ((slot (c2mop:slot-definition-name slot)))
             (setf (slot-value esd slot) (slot-value dslotd slot))))
         esd))
      (has-many-direct-slot-definition
       (let* ((*effective-slot-definition-class* 'has-many-effective-slot-definition)
              (esd (call-next-method)))
         (dolist (slot (c2mop:class-slots (class-of dslotd)))
           (let ((slot (c2mop:slot-definition-name slot)))
             (setf (slot-value esd slot) (slot-value dslotd slot))))
         esd))
      (belongs-to-direct-slot-definition
       (let* ((*effective-slot-definition-class* 'belongs-to-effective-slot-definition)
              (esd (call-next-method)))
         (dolist (slot (c2mop:class-slots (class-of dslotd)))
           (let ((slot (c2mop:slot-definition-name slot)))
             (setf (slot-value esd slot) (slot-value dslotd slot))))
         esd))
      (t (setf *effective-slot-definition-class* nil)
       (call-next-method)))))


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

(defun joins (&rest tables)
  (apply #'%joins *connection* *association* tables)
  *association*)

(defun order (order)
  (%order *connection* *association* order)
  *association*)

(defun where (&rest args)
  (apply #'%where *connection* *association* args)
  *association*)

(defun as-first ()
  (%as-first *connection* *association*))

(defun as-list ()
  (%as-list *connection* *association*))

(defun as-sql ()
  (%as-sql *connection* *association*))

(defgeneric %jonis (connection association &rest args))
(defgeneric %order (connection association order))
(defgeneric %where (connection association &rest args))
(defgeneric %as-first (connection association))
(defgeneric %as-list (connection association))

(defmethod %joins (connection association &rest args)
  (push args (slot-value association 'joins)))

(defmethod %order (connection association order)
  (push order (slot-value association 'orders)))

(defmethod %where (connection association &rest args)
  (push args (slot-value association 'wheres)))

(defmacro with-ar ((table) &body body)
  `(let ((*association* (ensure-association ,table)))
     ,@body))

(defmethod %as-sql-joins (connection association (join string) out)
  (write-char #\space out)
  (write-string join out))

(defmethod %as-sql-joins (connection association (join symbol) out)
  (let ((slot (find-slot-by-%key (base-of association) join)))
    (%as-sql-joins connection association slot out)))

(defmethod %as-sql-joins (connection association (slot belongs-to-effective-slot-definition) out)
  (let ((base-table-name (table-name-of (base-of association)))
        (join-table-name (table-name-of (association-class-of slot))))
    (format out " inner join ~a on ~:{~a.~a = ~a.~a~^ and~}"
            join-table-name
            `(,(list base-table-name (foreign-key-of slot) join-table-name (primary-key-of slot))
              ,@(when (polymorphic-of slot)
                  `(,(list base-table-name (foreign-type-of slot)
                           (to-camel (class-name (class-of (association-class-of slot)))))))))))

(defmethod %as-sql-joins (connection association (slot has-many-effective-slot-definition) out)
  (break "~a" slot)
  (format out " inner join ~a on" slot))

(defmethod %as-sql-where (connection association (where string) out)
  (write-string where out))
(defmethod %as-sql-where (connection association (where list) out)
  (destructuring-bind (sql &rest args) where
    (let ((sql (if (symbolp sql) (to-sql-token sql) sql)))
      (write-string
       (if args
           (if (find #\? sql)
               (ppcre:regex-replace-all "\\?" sql (sanitize-sql (car args)))
               (aif (ppcre:all-matches-as-strings ":[\\W]+" sql)
                    (loop for parameter in it
                          for value = (sanitize-sql (getf args (to-keyword parameter)))
                          do (setf sql (ppcre:regex-replace-all (ppcre:quote-meta-chars parameter)
                                                                sql
                                                                value))
                          finally (return sql))
                    (format nil "~a ~a ~a" sql (if (and (null (cdr args))
                                                        (atom (car args)))
                                                   "="
                                                   "in")
                            (apply #'sanitize-sql args))))
           sql)
       out))))


(defmethod %as-sql (connection association)
  (with-slots (selects joins wheres orders groups) association
    (let ((table-name (table-name-of (base-of association))))
      (with-output-to-string (out)
        (write-string "select" out)
        (if selects
            (collect-ignore
             (progn
               (write-char #\space out)
               (write-string (to-sql-token (scan-lists-of-lists selects)) out)))
            (format out " ~a.*" table-name))
        (format out " from ~a" table-name)
        (when joins
          (collect-ignore (%as-sql-joins connection association
                                         (scan-lists-of-lists-fringe (reverse joins)) out)))
        (when wheres
          (write-string " where" out)
          (collect-ignore
           (progn
             (write-string (latch (series " ") :after 1 :post " and ") out)
             (%as-sql-where connection association (scan (reverse wheres)) out))))
        (when orders
          (write-string " order by " out)
          (format out "~{~a~^, ~}" (mapcar #'to-sql-token orders)))))))

(defgeneric load-query-result (connection association row fields))
(defmethod load-query-result (connection association row fields)
  (apply #'make-instance (base-of association)
         :new-record nil
         (collect-append
             (list (to-keyword (scan fields)) (scan row)))))

(defmethod %as-first (connection association)
  (multiple-value-bind (rows fields)
      (clsql:query (%as-sql connection association))
    (load-query-result connection association (car rows) fields)))

(defmethod %as-list (connection association)
  (multiple-value-bind (rows fields) (query (%as-sql connection association))
    (mapcar (lambda (row)
              (load-query-result connection association row fields))
            rows)))

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
    ((new-record :initarg :new-record :initform t :accessor new-record-p)
     (dirty-hash :initform (make-hash-table :test #'eq)))
    (:metaclass active-record-class)))

(defmethod ensure-association ((association association))
  association)

(defmethod ensure-association (base)
  (make-instance 'association :base base))

(defmethod ensure-association ((association symbol))
  (ensure-association (find-class association)))


(defmethod table-name-of ((x base))
  (table-name-of (class-of x)))

(defmethod table-name-of ((x symbol))
  (table-name-of (find-class x)))

(defgeneric value-of (record column)
  (:method (record (column symbol))
    (slot-value record (c2mop:slot-definition-name (find-column record column))))
  (:method (record (column string))
    (slot-value record (c2mop:slot-definition-name (find-column record column))))
  (:method (record (column ar-slot-mixin))
    (slot-value record (c2mop:slot-definition-name column))))

(defgeneric (setf value-of) (value record column)
  (:method (value record (column symbol))
    (setf (slot-value record (c2mop:slot-definition-name (find-column record column)))
          value))
  (:method (value record (column string))
    (setf (slot-value record (c2mop:slot-definition-name (find-column record column)))
          value))
  (:method (value record (column ar-slot-mixin))
    (setf (slot-value record (c2mop:slot-definition-name column))
          value)))

(defgeneric find-column (class-or-instance string-or-symbol)
  (:method ((base base) string-or-symbol)
    (find-column (class-of base) string-or-symbol))
  (:method ((class active-record-class) string-or-symbol)
    (collect-first (choose-if (lambda (slot)
                                (string= string-or-symbol (c2mop:slot-definition-name slot)))
                              (scan-column-slot class)))))

(defgeneric columns-expect-id (active-record-class)
  (:method ((class active-record-class))
    (collect
        (choose-if (complement (lambda (slot)
                                 (string= :id (c2mop:slot-definition-name slot))))
                   (scan-column-slot class)))))

(defgeneric save (record)
  (:method-combination active-record)
  (:method ((self base))
    (create-or-update self)))

(defmethod save :after (record)
  (clrhash (slot-value record 'dirty-hash))
  (setf (new-record-p record) nil))

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

(defgeneric create-or-update (record)
  (:method-combination active-record)
  (:method ((self base))
    (if (new-record-p self)
        (create self)
        (update self))))

(defgeneric create (record)
  (:method-combination active-record)
  (:method ((self base))
    (let ((slots (columns-expect-id (class-of self))))
      (query
       (format nil "insert into ~a (~{~a~^,~}) values (~{~a~^,~})"
               (table-name-of self)
               (mapcar #'column-name-of slots)
               (mapcar (lambda (slot)
                         (to-sql-value (slot-value self (c2mop:slot-definition-name slot))))
                       slots)))
      (setf (new-record-p self) t
            (value-of self :id) (caar (query "select last_insert_id()"))))
    self))

(defgeneric update-sql (record)
  (:method ((self base))
    (with-slots (dirty-hash) self
      (if (zerop (hash-table-size dirty-hash))
          nil
          (format nil "update ~a set ~{~a = ~a~^,~} where id = ~a"
                  (table-name-of self)
                  (collect-append
                   (multiple-value-bind (slot value) (scan-hash dirty-hash)
                     (list (column-name-of slot)
                           (to-sql-value value))))
                  (value-of self :id))))))

(defgeneric update (record)
  (:method-combination active-record)
  (:method ((self base))
    (query (update-sql self))
    self))

(defgeneric destroy (record)
  (:method-combination active-record)
  (:method ((self base))
    (clsql-sys:execute-command (format nil "delete from ~a where id = ~d"
                                       (table-name-of (class-of self))
                                       (value-of self :id)))))


;; TODE default value
(defun column-to-slot-definition (name type precision scale nullable &optional default)
  (let ((slot-name (to-column-symbol name)))
    `(,slot-name
      :column-name ,name
      :initarg ,(to-keyword slot-name)
      :initform ,(and (not nullable) default)
      :accessor ,(to-accessor-symbol name)
      :sql-type ,type
      :precision ,precision
      :scale ,scale
      :nullable ,nullable)))


(defgeneric fetch-association (class instance slot))

(defmethod fetch-association (class instance (slot has-many-effective-slot-definition))
  (with-slots (association-class foreign-key foreign-type primary-key order) slot
    (with-ar ((find-class association-class))
      (where foreign-key (slot-value instance (to-lisp-token primary-key)))
      (and foreign-type (where foreign-type (to-camel (class-name class))))
      (and order (order order))
      (as-list))))

(defmethod fetch-association ((class active-record-class) instance (slot belongs-to-effective-slot-definition))
  (with-slots (association-class polymorphic foreign-key foreign-type primary-key) slot
    (with-ar ((find-class (if polymorphic
                              (find-symbol (string-upcase (slot-value instance (to-lisp-token foreign-type))))
                              association-class)))
      (where primary-key (slot-value instance (to-lisp-token foreign-key)))
      (as-first))))

(defmethod c2mop:slot-value-using-class ((class active-record-class) instance (slot has-many-effective-slot-definition))
  (let ((slot-name (c2mop:slot-definition-name slot)))
    (if (slot-boundp instance slot-name)
        (call-next-method)
        (setf (slot-value instance slot-name)
              (fetch-association class instance slot)))))

(defmethod c2mop::slot-value-using-class ((class active-record-class) instance (slot belongs-to-effective-slot-definition))
  (let ((slot-name (c2mop:slot-definition-name slot)))
    (if (slot-boundp instance slot-name)
        (call-next-method)
        (setf (slot-value instance slot-name)
              (fetch-association class instance slot)))))

(defmethod (setf c2mop:slot-value-using-class) :before
    (new-value
     (class active-record-class)
     instance
     (slot-def column-effective-slot-definition))
  (with-slots (dirty-hash) instance
    (unless (slot-boundp instance 'dirty-hash)
      (setf dirty-hash (make-hash-table :test #'eq)))
    (let ((slot-name (c2mop:slot-definition-name slot-def)))
      (when (slot-boundp instance slot-name)
        (unless (sql-value-equal (slot-value instance slot-name)
                                 new-value)
          (setf (gethash slot-def dirty-hash)
                new-value))))))

(defmethod (setf c2mop:slot-value-using-class) :after
    (new-value
     (class active-record-class)
     instance
     (slot-def has-many-effective-slot-definition))
  (with-slots (association-class foreign-key primary-key) slot-def
    (loop with id = (slot-value instance (to-lisp-token primary-key))
          with slot = (to-lisp-token foreign-key)
          for x in new-value
          do (setf (slot-value x slot) id))))

(defmethod (setf c2mop:slot-value-using-class) :after
    (new-value
     (class active-record-class)
     instance
     (slot-def belongs-to-effective-slot-definition))
  (when new-value
    (with-slots (association-class foreign-key foreign-type primary-key) slot-def
      (setf (slot-value instance (to-lisp-token foreign-key))
            (slot-value new-value (to-lisp-token primary-key)))
      (when foreign-type
        (setf (slot-value instance (to-lisp-token foreign-type))
              (to-camel (class-name (class-of instance))))))))


(defgeneric compute-slot-definition-from-class-option (table-name keyword &rest args)
  (:method (table-name any &rest args)
    (declare (ignore args))
    nil)
  (:method (table-name (keyword (eql :has-many)) &rest args)
    "(:has-many :experiences :order 'id desc' :dependent :destroy"
    (apply #'compute-has-many-slot-definition table-name args))
  (:method (table-name (keyword (eql :belongs-to)) &rest args)
    "(:belongs-to :parent)"
    (apply #'compute-belongs-to-slot-definition table-name args)))

(defun compute-has-many-slot-definition
    (table-name
     &key
       has-many
       (accessor (to-accessor-symbol has-many))
       (association-class (to-class has-many))
       as
       (foreign-key (format nil "~a_id" (if as
                                            (to-sql-token as)
                                            (singularize table-name)) ))
       (foreign-type (and as (format nil "~a_type" (to-sql-token as))))
       (primary-key "id")
       (dependent nil)
       order)
  "(:has-many :experiences :order 'id desc' :dependent :destroy"
  (values has-many
          (list (intern (symbol-name has-many) *package*)
                :%key has-many
                :has-many has-many
                :initarg has-many
                :accessor accessor
                :association-class association-class
                :as as
                :foreign-key foreign-key
                :foreign-type foreign-type
                :primary-key primary-key
                :dependent dependent
                :order order)))

(defun compute-belongs-to-slot-definition
    (table-name
     &key
       belongs-to
       (accessor (to-accessor-symbol belongs-to))
       (association-class (to-class belongs-to))
       polymorphic
       (foreign-key (str (to-sql-token belongs-to) "_id"))
       (foreign-type (and polymorphic (str (singularize (to-sql-token belongs-to)) "_type" )))
       (primary-key "id")
       (dependent nil))
  "(:belongs-to :parent)"
  (declare (ignore table-name))
  (values belongs-to
          (list (intern (symbol-name belongs-to) *package*)
                :%key belongs-to
                :belongs-to belongs-to
                :initarg belongs-to
                :accessor accessor
                :association-class association-class
                :polymorphic polymorphic
                :foreign-key foreign-key
                :foreign-type foreign-type
                :primary-key primary-key
                :dependent dependent)))

(defun compute-slot-definitions-from-class-options (table-name options)
  (let ((slot-definitions ()))
    (dolist (option options)
      (multiple-value-bind (key slot-definition)
          (apply #'compute-slot-definition-from-class-option
                 table-name (car option) option)
        (when key
          (push slot-definition slot-definitions))))
    slot-definitions))


(defmacro defrecord (class super-classes slots &rest options)
  (let* ((table-name (or (cadr (assoc :table-name options))
                         (let ((table-name (to-table-name class)))
                           (push `(:table-name ,table-name) options)
                           table-name)))
         (attribute-types (clsql-sys:list-attribute-types table-name))
         (relation-slot-definitions (compute-slot-definitions-from-class-options table-name options)))
    `(progn

       (defparameter ,class
         (defclass ,class ,(or super-classes '(base))
           (,@slots
            ,@(mapcar (lambda (x) (apply #'column-to-slot-definition x)) attribute-types)
            ,@relation-slot-definitions)
           (:metaclass active-record-class)))

       (setf (table-name-of ,class) ,table-name)

       ,class)))

(c2cl:finalize-inheritance base)

#|
(progn
  (establish-connection)

  (defrecord prefecture ()
    ()
    (:has-many :facilities))

  (defrecord facility ()
    ()
    (:belongs-to :prefecture)
    (:has-many :experiences :as :experiencable))

  (defrecord experience ()
    ()
    (:belongs-to :experiencable :polymorphic t))
  )

(let ((facilities (with-ar (facility)
                    (where "name like ?" "%水族館")
                    (where :publish 1)
                    (order :name)
                    (as-list))))
  (values
   (mapcar #'name-of facilities)
   (name-of (prefecture-of (car facilities)))))

(facilities-of (with-ar (prefecture)
                 (where "id = 1")
                 (as-first)))

(experiences-of (with-ar (facility)
                  (as-first)))

(experiencable-of (with-ar (experience)
                    (as-first)))

(with-ar (facility)
  (joins :prefecture)
  (where :prefectures.name "鳥取県")
  (as-list))
|#
