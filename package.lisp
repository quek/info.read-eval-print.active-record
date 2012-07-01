;;;; package.lisp

(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.active-record
 (:use :cl :anaphora :hu.dwim.defclass-star)
 (:shadow :type-of)
 (:export
  #:as-first
  #:as-list
  #:as-sql
  #:create
  #:defrecord
  #:establish-connection
  #:joins
  #:order
  #:query
  #:save
  #:sql-value=
  #:sql-value<
  #:sql-value>
  #:sql-value<=
  #:sql-value>=
  #:sql-value/=
  #:where
  #:with-ar
  ))
