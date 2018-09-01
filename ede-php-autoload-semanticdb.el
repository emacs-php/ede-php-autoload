;;; ede-php-autoload-semanticdb.el --- Semanticdb support for ede-php-autoload

;; Copyright (C) 2015, Steven Rémot

;; Author: Steven Rémot <steven.remot@gmail.com>
;;         Inspired by Joris Stein's edep <https://github.com/jorissteyn/edep>
;; Keywords: PHP project ede
;; Homepage: https://github.com/emacs-php/ede-php-autoload

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This provides a simple semanticdb backend that uses ede-php-autoload to
;; get tags by emulating PHP autoload system.

;;; Code:

(require 'semantic/db)
(require 'ede-php-autoload)

(eval-and-compile
  (unless (fboundp 'cl-defmethod)
    (defalias 'cl-defmethod 'defmethod))
  (unless (fboundp 'cl-call-next-method)
    (defalias 'cl-call-next-method 'call-next-method)))

(defclass ede-php-autoload-semanticdb-table (semanticdb-search-results-table eieio-singleton)
  ((major-mode :initform php-mode))
  "Database table for PHP using `ede-php-autoload'.")

(defclass ede-php-autoload-semanticdb-database (semanticdb-project-database eieio-singleton)
  ((new-table-class :initform ede-php-autoload-semanticdb-table
                    :type class
                    :documentation "Class of the new tables created for this database."))
  "Semanticdb database that uses `ede-php-autoload'.")

(cl-defmethod semanticdb-get-database-tables ((obj ede-php-autoload-semanticdb-database))
  "For an `ede-php-autoload-project', there is only one singleton table."
  (when (or (not (slot-boundp obj 'tables))
            (not (ede-php-autoload-semanticdb-table-p (car (oref obj tables)))))
    (let ((newtable (ede-php-autoload-semanticdb-table "EDE-PHP-AUTOLOAD")))
      (oset obj tables (list newtable))
      (oset newtable parent-db obj)))
  (cl-call-next-method))

(cl-defmethod semanticdb-file-table ((obj ede-php-autoload-semanticdb-database) filename)
  "For an `ede-php-autoload-project', use the only table."
  (car (semanticdb-get-database-tables obj)))

(defun ede-php-autoload-semanticdb-import-file-content-for-class (project class-name)
  "Import the tags in the file that defines a certain class.

PROJECT is the ede php project in which class is defined.
CLASS-NAME is the name of the class.

Return nil if it could not find the file or if the file was the current file."
  (let ((file (ede-php-autoload-find-class-def-file project class-name)))
    (when (and file (not (string= file (buffer-file-name))))
      (find-file-noselect file)
      (semanticdb-file-stream file))))

(defun ede-php-autoload-current-project ()
  "Return the current `ede-php-autoload' project."
  (when (ede-php-autoload-project-p (ede-current-project))
    (ede-current-project)))

(cl-defmethod semanticdb-find-tags-by-name-method
  ((table ede-php-autoload-semanticdb-table) name &optional tags)
  "Find all tags named NAME in TABLE"
  (if (ede-php-autoload-current-project)
      (or (ede-php-autoload-semanticdb-import-file-content-for-class
           (ede-php-autoload-current-project)
           name)
          (cl-call-next-method))
    (cl-call-next-method)))

(cl-defmethod semanticdb-deep-find-tags-by-name-method
  ((table ede-php-autoload-semanticdb-table) name &optional tags)
  "Find all tags name NAME in TABLE.
Optional argument TAGS is a list of tags to search.
Like `semanticdb-find-tags-by-name-method' for global."
  (semanticdb-find-tags-by-name-method table name tags))

(defun ede-php-autoload-semanticdb--wrap-suggestion-in-tag (suggestion)
  "Wrap the type completion SUGGESTION in a type tag."
  (semantic-tag-new-type suggestion 'unknown '() '()))

(cl-defmethod semanticdb-find-tags-for-completion-method
  ((table ede-php-autoload-semanticdb-table) prefix &optional tags)
  "In TABLE, find all occurrences of tags matching PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a table of all matching tags."
  (if (ede-php-autoload-current-project)
      (mapcar #'ede-php-autoload-semanticdb--wrap-suggestion-in-tag
              (ede-php-autoload-complete-type-name (ede-php-autoload-current-project) prefix))))

(provide 'ede-php-autoload-semanticdb)

;;; ede-php-autoload-semanticdb.el ends here
