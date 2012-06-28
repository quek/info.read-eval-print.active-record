(in-package :info.read-eval-print.active-record)

(ql:quickload :fiveam)

(setf 5am:*debug-on-failure* t
      5am:*debug-on-error* t)

(5am:def-suite ar)

(5am:in-suite ar)

(establish-connection '("localhost" "outing_development" "root" ""))

(5am:test defrecord
  ""
  (5am:is (defrecord region ()
            ()
            (:has-many :prefectures)))
  (5am:is (string= "関東" (with-ar (region)
                            (where :id 3)
                            (name-of (get-first)))))
  (5am:is (defrecord prefecture ()
            ()
            (:belogs-to :region)))
  (5am:is (string= "東京都" (with-ar (prefecture)
                              (where :id 13)
                              (name-of (get-first))))))

(5am:run! 'ar)
