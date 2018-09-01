;;; ede-php-autoload-composer.el --- Composer projects detection and analysis -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2017, Steven Rémot

;; Author: Steven Rémot <steven.remot@gmail.com>
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
;;

(require 'json)

;;; Code:

;;;###autoload
(defconst ede-php-autoload-composer-file "composer.json"
  "File name for composer configuration.")

(defconst ede-php-autoload-composer-lock-file "composer.lock"
  "File name for the composer dependecies lock file.")

(defvar ede-php-autoload-composer--visitors nil
  "The visitors that will be executed to generate composer autoloads.")

(defun ede-php-autoload--format-composer-single-dir (namespace path base-dir standard)
  "Format a composer autoload pair when the path is a single string.

NAMESPACE is the autoloaded namespace.

PATH is a string representing the relative directory.

BASE-DIR is the directory PATH is relative to.

STANDARD is the autoload standard (e.g. `psr-0')."

  (when base-dir
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

    (when base-dir
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

         (last-character (if (string= namespace "")
                             nil
                           (aref namespace (1- (length namespace)))))

         (path (cdr pair)))

    (when (member last-character '(?\\ ?_))
      (setq namespace (substring-no-properties namespace 0 (1- (length namespace)))))

    (if (stringp path)
        (ede-php-autoload--format-composer-single-dir namespace path base-dir standard)
      (ede-php-autoload--format-composer-multiple-dirs namespace path base-dir standard))))

(defun ede-php-autoload-composer-create-autoloads-from-data (composer-data &optional base-dir)
  "Return internal autoloads from a composer.json file data.

COMPOSER-DATA is the parsed content of a composer.json file.
BASE-DIR is the prefix dir to add to each autoload path."
  (let ((composer-autoloads (append (cdr (assoc 'autoload composer-data))
                                    (cdr (assoc 'autoload-dev composer-data))))
        (autoloads '())
        key spec)

    (dolist (autoload-part composer-autoloads)
      (when (member (car autoload-part) '(psr-0 psr-4))
        (setq key (intern (concat ":" (symbol-name (car autoload-part))))

              spec (mapcar #'(lambda (pair)
                               (ede-php-autoload--format-composer-pair pair base-dir (car autoload-part)))
                           (cdr autoload-part))

              autoloads (plist-put autoloads
                                   key
                                   (append (plist-get autoloads key) spec)))))

    autoloads))

(defun ede-php-autoload-composer--merge-autoload-paths (base-paths new-paths)
  "Merge two paths in a autoload file in one.

BASE-PATHS and NEW-PATHS are either string opr list of strings.

It will output a list of strings with each of base and new paths in it."
  (let ((list-base-path (if (stringp base-paths) (list base-paths) base-paths))
        (list-new-paths (if (stringp new-paths) (list new-paths) new-paths)))
    (append list-base-path list-new-paths)))

(defun ede-php-autoload-composer--merge-autoload-entries (base-entries new-entries)
  "Merge two autoload entries (right after the autoload entry).

BASE-ENTRIES and NEW-ENTRIES are the entries to merge."
  (let ((current-entries base-entries)
        pair
        pair-path)
    (cl-loop for (ns . paths) in new-entries do
             (setq pair (assoc ns current-entries)
                   pair-path (cdr pair))
             (if pair
                 (setf (cdr pair) (ede-php-autoload-composer--merge-autoload-paths pair-path paths))
               (setq current-entries (push (cons ns paths) current-entries))))
    current-entries))

(defun ede-php-autoload-composer-merge-autoloads (base-autoloads new-autoloads)
  "Merge two internal autoload definitions in one.

BASE-AUTOLOADS and NEW-AUTOLOADS are two internal autoload lists.

NEW-AUTOLOADS will be merged into BASE-AUTOLOADS.  BASE-AUTOLOADS will be mutated."
  (let ((autoloads base-autoloads)
        (index 0)
        (new-autoloads-length (length new-autoloads))
        key value)

    (while (< index new-autoloads-length)
      (setq key (nth index new-autoloads)

            value (nth (1+ index) new-autoloads)

            autoloads (plist-put autoloads
                                 key
                                 (ede-php-autoload-composer--merge-autoload-entries
                                  (plist-get autoloads key)
                                  value))

            index (+ index 2)))

    autoloads))

(defun ede-php-autoload-composer-merge-composer-autoloads (composer-data autoloads &optional base-dir)
  "Load the autoload information in COMPOSER-DATA and merge it with AUTOLOADS.

COMPOSER-DATA is the parsed composer.json file.
BASE-DIR is the prefix dir to add to each autoload path."
  (ede-php-autoload-composer-merge-autoloads autoloads (ede-php-autoload-composer-create-autoloads-from-data composer-data base-dir)))

(defun ede-php-autoload-composer--get-data (dir)
  "Return the parsed composer.json file in DIR if any.

Return nil otherwise."
  (let ((composer-file (expand-file-name ede-php-autoload-composer-file dir)))
    (when (file-exists-p composer-file)
      (json-read-file composer-file))))

(defun ede-php-autoload-composer--get-third-party-data (composer-lock)
  "Return the composer packages in composer.lock file.

COMPOSER-LOCK is the content of the composer.lock file."
  (if composer-lock
      (vconcat (cdr (assoc 'packages composer-lock))
               (cdr (assoc 'packages-dev composer-lock)))
    []))

(defun ede-php-autoload-composer--get-third-party-dir (package-data vendor-dir)
  "Return the directory that contain third party sources.

PACKAGE-DATA is the data for the corresponding third-party in the
composer.lock file.

VENDOR-DIR is the project's vendor directory."
  (expand-file-name (cdr (assoc 'name package-data)) vendor-dir))

;; Visitor system ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ede-php-autoload-composer-make-context (composer-data composer-lock project-dir)
  "Create the context in which the composer project is defined.

COMPOSER-DATA is the parsed content of the composer.json file.

COMPOSER-LOCK is the parsed content of the composer.lock file.

PROJECT-DIR is the absolute path of the project directory."
  `((composer-data . ,composer-data)
    (composer-lock . ,composer-lock)
    (project-dir . ,project-dir)))

(defun ede-php-autoload-composer-get-composer-data (context)
  "Return the content of the composer.json file in the given CONTEXT."
  (cdr (assoc 'composer-data context)))

(defun ede-php-autoload-composer-get-composer-lock (context)
  "Return the content of the composer.lock file in the given CONTEXT."
  (cdr (assoc 'composer-lock context)))

(defun ede-php-autoload-composer-get-project-dir (context)
  "Return the absolute path to the project directory in the given CONTEXT."
  (cdr (assoc 'project-dir context)))

(defun ede-php-autoload-composer-define-visitor (visitor &optional step)
  "Add a new VISITOR to the list of composer visitors.

A visitor is a function that takes a context and the current list
of autoloads, and returns a new list of autoloads.

All visitors are executed when a composer project is detected, to
generate the composer autoloads.

STEP is the autoload construction step at which the visitor
should execute.  It can be `:early', `:normal' or `:late.  It
defaults to `:normal'.'"
  (let* ((real-step (or step :normal))
         (pair (assoc real-step ede-php-autoload-composer--visitors)))
    (if pair
        (setf (cdr pair) (push  visitor (cdr pair)))
      (add-to-list 'ede-php-autoload-composer--visitors (cons real-step (list visitor))))))

(defun ede-php-autoload-composer--run-visitors (visitors context autoloads)
  "Run all the visitors on a specified CONTEXT, with the initial AUTOLOADS.

Returns the new list of autoloads."
  (let ((current-autoloads autoloads)
        step-visitors)

    (dolist (step '(:early :normal :late))
      (setq step-visitors (cdr (assoc step visitors)))
      (dolist (visitor step-visitors)
        (setq current-autoloads (funcall visitor context current-autoloads))))

    current-autoloads))

;; Basic visitors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ede-php-autoload-composer--merge-composer-data-autoloads (context autoloads)
  "Load the autoload information in composer.json and merge it with autoloads.

CONTEXT is the composer context.
AUTOLOADS is the current list of autoloads."
  (ede-php-autoload-composer-merge-composer-autoloads (ede-php-autoload-composer-get-composer-data context) autoloads nil))

(ede-php-autoload-composer-define-visitor #'ede-php-autoload-composer--merge-composer-data-autoloads)

(defun ede-php-autoload-composer--merge-composer-lock-autoloads (context autoloads)
  "Load the autoload information from lock file and merge it with autoloads.

CONTEXT is the composer context.
AUTOLOADS is the current list of autoloads."
  (let* ((project-dir (ede-php-autoload-composer-get-project-dir context))
         (composer-lock (ede-php-autoload-composer-get-composer-lock context))
         (third-party-data (ede-php-autoload-composer--get-third-party-data composer-lock))
         (vendor-dir (expand-file-name "vendor" project-dir))
         (i 0)
         (l (length third-party-data))
         current-data)
    (while (< i l)
      (setq current-data (aref third-party-data i)
            autoloads (ede-php-autoload-composer-merge-composer-autoloads
                       current-data
                       autoloads
                       (ede-php-autoload-composer--get-third-party-dir current-data vendor-dir))
            i (1+ i)))
    autoloads))

(ede-php-autoload-composer-define-visitor #'ede-php-autoload-composer--merge-composer-lock-autoloads)

;; Entry point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ede-php-autoload--append-composer-autoload-data (project-dir autoloads)
  "Add all composer autoload information.

If PROJECT-DIR has a composer specification, add its autoload
information into AUTOLOADS."
  (let* ((composer-data (ede-php-autoload-composer--get-data project-dir))
         (lock-file (expand-file-name ede-php-autoload-composer-lock-file project-dir))
         (composer-lock (when (file-exists-p lock-file) (json-read-file lock-file)))
         (context (ede-php-autoload-composer-make-context composer-data composer-lock project-dir)))
    (ede-php-autoload-composer--run-visitors ede-php-autoload-composer--visitors context autoloads)))


(provide 'ede-php-autoload-composer)

;;; ede-php-autoload-composer.el ends here
