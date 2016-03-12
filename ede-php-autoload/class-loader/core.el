;;; core.el --- Base primitives for class loader definition

;; Copyright (C) 2016, Steven Rémot

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
;;; Code:
(require 'eieio)

(defclass ede-php-autoload-class-loader ()
  ()
  "Base class for finding the file in with some class is defined."
  :abstract t)

(defmethod ede-php-autoload-find-class-def-file ((this ede-php-autoload-class-loader)
                                             class-name)
  "Find the file in which CLASS-NAME is defined.

CLASS-NAME must be the full name of the class, with all its parent namespaces."
  (error "Method `ede-php-autoload-find-class-def-file' must be overriden"))

(defmethod ede-php-autoload-get-class-name-for-file
  ((this ede-php-autoload-class-loader) file-name)
  "Generate a suitable class name for the current FILE-NAME.

Generate this class name using the class loader information.

FILE-NAME must be absolute or relative to the project root."
  (error "Method `ede-php-autoload-find-class-def-file' must be overriden"))

(defmethod ede-php-autoload-complete-type-name ((this ede-php-autoload-class-loader) prefix)
  "Get completion suggestions for the type PREFIX.

PREFIX is the beginning of a fully-qualified name.

The result is a list of completion suggestions for this
prefix. Completions are not guaranteed to give full class names,
this can only suggest the next namespace."
  '())

;;; Utility functions for loaders

(defun ede-php-autoload--get-path-relative-to-ns (class-name namespace &optional extension)
  "Return the path of the class file relative to the namespace directory.

CLASS-NAME is the class name.

NAMESPACE is the namespace to map.

EXTENSION is the file extension to put at the end of the file, \".php\" by default.

Example: (ede-php-autoload--get-path-relative-to-ns \"My\\Ns\\My\\Class\" \"My\\Ns\")
         => \"My/Class.php\""
  (concat
   (mapconcat
    'identity
    (nthcdr (length (split-string namespace (rx (or "\\" "_")) t))
            (split-string class-name (rx (or "\\" "_")) t))
    "/")
   (or extension ".php")))

(defun ede-php-autoload--gather-relative-subfiles (ns-directories project-root relative-path prefix)
  "Return all relative file names in namespace subdirectories.

NS-DIRECTORIES are list of directories for a namespace, relative
to the PROJECT-ROOT.

RELATIVE-PATH is the path to browser in each NS-DIRECTORIES.

Only files starting with PREFIX will be kept.

Basically, it returns PROJECT-ROOT/{NS-DIRECTORIES}/RELATIVE-PATH/{PREFIX}*"
  (let ((files '())
        absolute-dir
        full-dir)
    (dolist (dir (ede-php-autoload--ensure-list ns-directories))
        (setq absolute-dir (if (file-name-absolute-p dir)
                               dir
                             (expand-file-name dir project-root))
              full-dir (expand-file-name relative-path absolute-dir)
              files (append files (directory-files
                                   full-dir
                                   nil
                                   (concat "^" (regexp-quote prefix))))))
    files))

(defun ede-php-autoload--ensure-list (list-or-element)
  "Ensure LIST-OR-ELEMENT will be wrapped in a list."
  (if (listp list-or-element) list-or-element (list list-or-element)))

(defun ede-php-autoload--search-in-dirs (file directories root)
  "Search for a FILE existing in one of the given DIRECTORIES.

DIRECTORIES are absolute paths or relative to ROOT."
  (let ((dirs (ede-php-autoload--ensure-list directories))
        existing-file
        candidate
        current-dir
        absolute-dir)
    (while (and (not existing-file) dirs)
      (setq current-dir (car dirs)

            absolute-dir (if (file-name-absolute-p current-dir)
                             current-dir
                           (expand-file-name current-dir root))

            candidate (expand-file-name file absolute-dir)
            dirs (cdr dirs))

      (when (file-regular-p candidate)
        (setq existing-file candidate)))

    existing-file))

;;
;; Factories
;;
;; Associate a keyword to a factory function that can create a class
;; loader from a certain configuration.
;;

(defvar ede-php-autoload-class-loader--factories '()
  "Association list mapping a keyword symbol to a factory function.")

(defun ede-php-autoload-class-loader--define-factory (name factory)
  "Register a new factory.

NAME is the keyword symbol referring the the factory.

FACTORY is a function taking as parameter a
configuration (usually a list), and returns an instance of
`ede-php-autoload-class-loader'."
  (add-to-list 'ede-php-autoload-class-loader--factories (cons name factory)))

(defmacro ede-php-autoload-class-loader-define-factory (name arguments &rest body)
  "Register a new factory.

A factory is a function taking a configuration value as argument
and returning an instance of`ede-php-autoload-class-loader'.

NAME is the keyword symbol referring the the factory.

ARGUMENTS is the list reprlesenting the function arguments.

BODY is the function implementation."
  (declare (debug (symbolp sexp body))
           (indent defun))
  `(ede-php-autoload-class-loader--define-factory ,name
                                                  (lambda ,arguments ,@body)))

(defun ede-php-autoload-class-loader-call-factory (name configuration)
  "Call a class loader factory with the specified configuration.

NAME is the keyword symbol representing the factory.

CONFIGURATION is the value sent to the factory.

Returns an instance of `ede-php-autoload-class-loader'."
  (let ((factory (cdr (assoc name ede-php-autoload-class-loader--factories))))
    (unless factory
      (error "Class loader factory %S not found" name))
    (funcall factory configuration)))

(provide 'ede-php-autoload/class-loader/core)

;;; core.el ends here
