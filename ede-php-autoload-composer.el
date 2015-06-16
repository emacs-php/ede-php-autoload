;;; ede-php-autoload-composer.el --- Composer projects detection and analysis -*- lexical-binding: t -*-

;; Copyright (C) 2015, Steven Rémot

;; Author: Steven Rémot <steven.remot@gmail.com>
;; Keywords: PHP project ede
;; Homepage: https://github.com/stevenremot/ede-php-autoload

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
;;

(require 'json)

;;; Code:

;;;###autoload
(defconst ede-php-autoload-composer-file "composer.json"
  "File name for composer configuration.")

(defconst ede-php-autoload-composer-lock-file "composer.lock"
  "File name for the composer dependecies lock file.")

(defun ede-php-autoload--format-composer-single-dir (namespace path base-dir standard)
  "Format a composer autoload pair when the path is a single string.

NAMESPACE is the autoloaded namespace.

PATH is a string representing the relative directory.

BASE-DIR is the directory PATH is relative to.

STANDARD is the autoload standard (e.g. `psr-0')."

    (when (stringp base-dir)
      (setq path (concat (file-name-as-directory base-dir) path)))

    (when (string= "psr-0" standard)
      (setq path (concat path (replace-regexp-in-string (rx "\\") "/"  namespace) "/")))

    (cons namespace path))

(defun ede-php-autoload--format-composer-multiple-dirs
    (namespace paths base-dir standard)
  "Format a composer autoload pair when the path is a single string.

NAMESPACE is the autoloaded namespace.

PATHS is a vector of strings representing the relative directories.

BASE-DIR is the directory PATHS are relative to.

STANDARD is the autoload standard (e.g. `psr-0')."
  (let  ((list-paths (append paths '())))

    (when (stringp base-dir)
      (mapc #'(lambda (path)
                (concat (file-name-as-directory base-dir) path))
            list-paths))

    (when (string= "psr-0" standard)
      (mapc #'(lambda (path)
                (concat path namespace "/"))
            list-paths))

    (cons namespace list-paths)))

(defun ede-php-autoload--format-composer-pair (pair base-dir standard)
  "Format composer autoload pair to `ede-php-autoload' format.

Remove the last character of composer autoload PAIR's namespace.

Ex: \"Foo\\\" => \"Foo\"

Composer needs it to perform its matches, but we do not need
it.

Then, if BASE-DIR is a string, prepend it to the autoload path.

STANDARD is either \"psr-0\" or \"psr-4\".  If STANDARD is
\"psr-0\", append namespace name to the path like composer does."
  (let* ((namespace (symbol-name (car pair)))
         (last-character (aref namespace (1- (length namespace))))
         (path (cdr pair)))

    (when (member last-character '(?\\ ?_))
      (setq namespace (substring-no-properties namespace 0 (1- (length namespace)))))

    (if (stringp path)
        (ede-php-autoload--format-composer-single-dir namespace path base-dir standard)
      (ede-php-autoload--format-composer-multiple-dirs namespace path base-dir standard))))

(defun ede-php-autoload--merge-composer-autoloads (composer-data autoloads &optional base-dir)
  "Load the autoload information in COMPOSER-DATA and merge it with AUTOLOADS.

COMPOSER-DATA is the parsed composer.json file.
BASE-DIR is the prefix dir to add to each autoload path."
  (let ((composer-autoloads (cdr (assoc 'autoload composer-data)))
        key spec
        base-spec)
    (dolist (autoload-part composer-autoloads)
      (when (member (car autoload-part) '(psr-0 psr-4))
        (setq key (intern (concat ":" (symbol-name (car autoload-part))))

              spec (mapcar #'(lambda (pair)
                               (ede-php-autoload--format-composer-pair pair base-dir (car autoload-part)))
                           (cdr autoload-part))

              base-spec (plist-get autoloads key))

        (setq autoloads (plist-put autoloads key (append base-spec spec)))))
    autoloads))

(defun ede-php-autoload-composer--get-data (dir)
  "Return the parsed composer.json file in DIR if any.

Return nil otherwise."
  (let ((composer-file (expand-file-name ede-php-autoload-composer-file dir)))
    (when (file-exists-p composer-file)
      (json-read-file composer-file))))

(defun ede-php-autoload-composer--get-third-party-data (project-dir)
  "Return the composer packages in composer.lock file.

PROJECT-DIR is the root directory."
  (let* ((lock-file (expand-file-name ede-php-autoload-composer-lock-file project-dir))
         (lock-file-data (when (file-exists-p lock-file) (json-read-file lock-file))))
    (if lock-file-data
        (cdr (assoc 'packages lock-file-data))
      [])))

(defun ede-php-autoload-composer--get-third-party-dir (package-data vendor-dir)
  "Return the directory that contain third party sources.

PACKAGE-DATA is the data for the corresponding third-party in the
composer.lock file.

VENDOR-DIR is the project's vendor directory."
  (expand-file-name (cdr (assoc 'name package-data)) vendor-dir))

(defun ede-php-autoload-composer--merge-lock-file-data (project-dir autoloads)
  "Merge the lock file content in the autoloads.

PROJECT-DIR is the project root.

AUTOLOADS is the current autoload configuration ot merge with."
  (let* ((third-party-data (ede-php-autoload-composer--get-third-party-data project-dir))
         (vendor-dir (expand-file-name "vendor" project-dir))
         (i 0)
         (l (length third-party-data))
         current-data)
    (while (< i l)
      (setq current-data (aref third-party-data i)
            autoloads (ede-php-autoload--merge-composer-autoloads
                       current-data
                       autoloads
                       (ede-php-autoload-composer--get-third-party-dir current-data vendor-dir))
            i (1+ i)))
    autoloads))

(defun ede-php-autoload--append-composer-autoload-data (project-dir autoloads)
  "Add all composer autoload information.

If PROJECT-DIR has a composer specification, add its autoload
information into AUTOLOADS."
  (let ((root-data (ede-php-autoload-composer--get-data project-dir)))
    (ede-php-autoload-composer--merge-lock-file-data
     project-dir
     (ede-php-autoload--merge-composer-autoloads root-data autoloads nil))))


(provide 'ede-php-autoload-composer)

;;; ede-php-autoload-composer.el ends here
