;;; psr4.el --- PSR-4 class loader implementation

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
;; Class loader for PSR-4 namespaces.
;;
;; To use it :
;;    (ede-php-autoload-project "My project"
;;                              :file "main.php"
;;                              :class-autoloads '(:psr-4 (("Psr4\\Ns" . "base/directory")
;;                                                         ("Psr4\\Ns2" . "base/directory2"))))
;;
;;
;;; Code:
(require 'ede)
(require 'ede-php-autoload/class-loader/core)

;;;###autoload
(defclass ede-php-autoload-psr4-class-loader (ede-php-autoload-class-loader)
  ((namespaces :initarg :namespaces
                  :initform ()
                  :documentation
                  "An associative list in which keys are namespaces, and  values are their include paths.

For example, if :namespaces has the value '((\"Foo\" . \"src/Foo\") (\"Bar\" . \"src/test/Bar\")),
then The class \"Bar\\Foo\" is considered to be defined in \"src/test/Bar/Foo\"."))
  "Class loader for PSR-4 convention.")

(defmethod ede-php-autoload-find-class-def-file ((this ede-php-autoload-psr4-class-loader)
                                             class-name)
  "Find the file in which CLASS-NAME is defined.

Return nil if no file has been found."
  (let* ((project-root (ede-project-root-directory (ede-current-project)))
         (namespaces (oref this namespaces))
         class-def-file)
    (while (and namespaces (not class-def-file))
      (let ((pair (car namespaces)))
        (when (string-prefix-p (car pair) class-name)
          (setq class-def-file (ede-php-autoload--search-in-dirs
                                (ede-php-autoload--get-path-relative-to-ns class-name (car pair))
                                (cdr pair)
                                project-root)))
        (setq namespaces (cdr namespaces))))
    class-def-file))

(defun ede-php-autoload--get-longest-prefix (pairs target)
  "Find the autoload pair which has the longest matching prefix of the target.

PAIRS is an associative list.

TARGET is a string."
  (let ((current-pairs pairs)
        extracted-list
        extracted
        longest-extracted
        longest-pair)
    (while current-pairs
      (setq extracted-list (ede-php-autoload--ensure-list
                            (cdar current-pairs)))
      (while extracted-list
        (setq extracted (car extracted-list))
        (when (and (string-prefix-p extracted target)
                   (or (null longest-pair)
                       (> (length extracted) (length longest-extracted))))
          (setq longest-extracted extracted
                longest-pair (cons (caar current-pairs) extracted)))
        (setq extracted-list (cdr extracted-list)))
      (setq current-pairs (cdr current-pairs)))
    longest-pair))

(defmethod ede-php-autoload-get-class-name-for-file
  ((this ede-php-autoload-psr4-class-loader) file-name)
  "Generate a suitable class name for the current FILE-NAME.

Generate this class name using the class loader information.

FILE-NAME must be absolute or relative to the project root."
  (let* ((project-root (ede-project-root-directory (ede-current-project)))
         (rel-file-name (if (file-name-absolute-p file-name)
                            (file-relative-name file-name project-root)
                          file-name))
         (associated-ns (ede-php-autoload--get-longest-prefix (oref this namespaces)
                                                              rel-file-name)))
    (when associated-ns
      (replace-regexp-in-string
       (rx "\\\\") (rx "\\")
       (mapconcat
        #'identity
        (list
         (car associated-ns)
         (replace-regexp-in-string (rx "/")
                                   (rx "\\")
                                   (substring (file-name-sans-extension file-name)
                                              (length (cdr associated-ns)))))
        "\\")))))

(defun ede-php-autoload--complete-for-psr4-pair (namespace directories project-root prefix)
  "Get completion suggestions for a PSR-4 loader pair.

NAMESPACE is the represented namespace.

DIRECTORIES is a list of directories associated to the namespace.

PROJECT-ROOT is the path to the project's root.

PREFIX is the beginning of the type to complete."
  (let ((list-directories (ede-php-autoload--ensure-list directories))
        (suggestions '())
        split-prefix
        relative-path
        absolute-dir
        full-dir)
    (cond
     ((string-prefix-p prefix namespace t)
      ;; If `prefix' is the beginning of `namespace', let's use
      ;; `namespace' as suggestion.
      (push (concat namespace "\\") suggestions))
     ((string-prefix-p namespace prefix)
      ;; If `prefix' starts with `namespace', let's use directory and
      ;; file structure to create suggestions
      (setq split-prefix (split-string prefix "\\\\")
            relative-path (file-name-as-directory
                           (ede-php-autoload--get-path-relative-to-ns
                            (mapconcat 'identity
                                       (butlast split-prefix)
                                       "\\")
                            namespace
                            "")))

      (setq suggestions (delete-dups
                         (delete nil
                                 (mapcar
                                  #'file-name-base
                                  (ede-php-autoload--gather-relative-subfiles
                                   directories
                                   project-root
                                   relative-path
                                   (car (last split-prefix)))))))))
    suggestions))

(defmethod ede-php-autoload-complete-type-name ((this ede-php-autoload-psr4-class-loader) prefix)
  "Get completion suggestions for the type PREFIX.

PREFIX is the beginning of a fully-qualified name.

The result is a list of completion suggestions for this
prefix. Completions are not guaranteed to give full class names,
this can only suggest the next namespace."
  (let ((project-root (ede-project-root-directory (ede-current-project)))
        (namespaces (oref this namespaces))
        (suggestions '())
        pair)
    (while namespaces
      (setq pair (car namespaces)
            suggestions (append suggestions
                                (ede-php-autoload--complete-for-psr4-pair
                                 (car pair)
                                 (cdr pair)
                                 project-root
                                 prefix))
            namespaces (cdr namespaces)))
    suggestions))

(ede-php-autoload-class-loader-define-factory :psr-4 (config)
  (ede-php-autoload-psr4-class-loader "PSR-4" :namespaces config))

(provide 'ede-php-autoload/class-loader/psr4)

;;; psr4.el ends here
