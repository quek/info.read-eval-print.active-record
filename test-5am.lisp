(ql:quickload :fiveam)
(ql:quickload :info.read-eval-print.active-record)

(defpackage :info.read-eval-print.active-record.test
  (:use :cl :info.read-eval-print.active-record))

(in-package :info.read-eval-print.active-record.test)


(setf 5am:*debug-on-failure* t
      5am:*debug-on-error* t)

(5am:def-suite ar)

(5am:in-suite ar)

(let ((db-name "active_record_test"))
  (asdf:run-shell-command
   "echo 'CREATE DATABASE IF NOT EXISTS `~a` DEFAULT CHARACTER SET utf8' | mysql -uroot"
   db-name)
  (establish-connection (list "localhost" db-name "root" "")))

(progn
  (query "drop table if exists `entries`")
  (query "create table entries (
  `id` int(11) not null auto_increment,
  `title` varchar(100) not null,
  `content` text not null,
  `created_at` datetime not null,
  `updated_at` datetime not null,
  primary key (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8")
  (query "drop table if exists `comments`")
  (query "create table comments (
  `id` int(11) not null auto_increment,
  `entry_id` int(11) not null,
  `content` text not null,
  `created_at` datetime not null,
  `updated_at` datetime not null,
  primary key (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8")
  (query "drop table if exists `tags`")
  (query "create table `tags` (
  `id` int(11) not null auto_increment,
  `name` varchar(100) not null,
  `created_at` datetime not null,
  primary key (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8")
  (query "drop table if exists `taggings`")
  (query "create table `taggings` (
  `id` int(11) not null auto_increment,
  `tag_id` int(11) not null,
  `taggable_id` int(11) not null,
  `taggable_type` varchar(255) not null,
  `created_at` datetime not null,
  primary key (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8")
  )

(5am:test defrecord
  ""
  (5am:is (defrecord entry ()
            ()
            (:has-many :comments)))
  (5am:is (defrecord comment ()
            ()
            (:belongs-to :entry))))

(5am:test save
  ""
  (let ((entry (make-instance 'entry :title "タイトル" :content "中味")))
    (5am:is (save entry))
    (let ((entry (with-ar (entry)
                   (where :id (id-of entry))
                   (get-first))))
      (5am:is (string= "タイトル" (title-of entry)))
      (5am:is (string= "中味" (content-of entry))))))


(5am:run! 'ar)


