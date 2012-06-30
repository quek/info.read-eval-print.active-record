(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :hu.dwim.stefil+hu.dwim.def+swank)
  (ql:quickload :info.read-eval-print.active-record))

(defpackage :info.read-eval-print.active-record.test
  (:use :cl :info.read-eval-print.active-record))

(in-package :info.read-eval-print.active-record.test)

(hu.dwim.stefil:defsuite* (info.read-eval-print.active-record.test
                           :in hu.dwim.stefil:root-suite))

(hu.dwim.stefil:defsuite* (info.read-eval-print.active-record.test.with-craete-tables
                           :in info.read-eval-print.active-record.test)
    nil
    (hu.dwim.stefil:with-fixture create-tables-fixture
      (hu.dwim.stefil:-run-child-tests-)))

(hu.dwim.stefil:defixture create-tables-fixture
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
  (hu.dwim.stefil:-body-))

(hu.dwim.stefil:deftest test-defrecord ()
  (hu.dwim.stefil:is
      (defrecord entry ()
        ()
        (:has-many :comments)))
  (hu.dwim.stefil:is
      (defrecord comment ()
        ()
        (:belongs-to :entry))))

(hu.dwim.stefil:deftest test-create ()
  (let ((new-entry (make-instance 'entry :title "タイトル" :content "中味")))
    (hu.dwim.stefil:is (save new-entry))
    (let ((load-entry (with-ar (entry)
                        (where :id (id-of new-entry))
                        (as-first))))
      (hu.dwim.stefil:is (string= "タイトル" (title-of load-entry)))
      (hu.dwim.stefil:is (string= "中味" (content-of load-entry))))))

(hu.dwim.stefil:deftest test-update ()
  (save (make-instance 'entry :title "update テスト" :content "なかみ"))
  (let* ((entry (with-ar (entry)
                  (where :title "update テスト")
                  (as-first)))
         (id (id-of entry)))
    (setf (title-of entry) "update しました"
          (content-of entry) "更新したなかみ")
    (save entry)
    (let ((entry (with-ar ('entry)
                   (where :id id)
                   (as-first))))
      (hu.dwim.stefil:is (string= "update しました" (title-of entry)))
      (hu.dwim.stefil:is (string= "更新したなかみ" (content-of entry)))))
  )


(info.read-eval-print.active-record.test)
