;;;; info.read-eval-print.active-record.asd

(asdf:defsystem #:info.read-eval-print.active-record
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "active-record"))
  :depends-on (#:clsql
               #:closer-mop
               #:contextl
               #:anaphora
               #:hu.dwim.defclass-star
               #:info.read-eval-print.series-ext))

