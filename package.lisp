;;;; package.lisp

(info.read-eval-print.series-ext:sdefpackage
 :info.read-eval-print.active-record
 (:use :cl :anaphora :hu.dwim.defclass-star)
 (:shadow :type-of)
 (:export #:establish-connection
          #:defrecord
          #:query
          #:create
          #:joins
          #:where
          #:order
          #:as-list
          #:as-first
          #:save
          #:with-ar))
