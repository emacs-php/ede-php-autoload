;;; ede-php-autoload.el --- Simple EDE PHP Project

;; Copyright (C) 2014, Steven Rémot

;; Author: Steven Rémot <steven.remot@gmail.com>
;;         original code for C++ by Eric M. Ludlam <eric@siege-engine.com>
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
;; Simple PHP project for EDE.  Inspired by `ede-cpp-root-project'.
;;
;; Example project definition :
;; (ede-php-autoload-project "My project"
;;                       :file "/path/to/a/file/at/root"
;;                       :class-autoloads '(:psr-0 (("MyNs" . "src/MyNs")
;;                                                 ("AnotherNs" . "src/AnotherNs"))
;;                                          :psr-4 (("MyModernNs" . "src/modern/MyNs"))))
;;
;; This EDE project can then be used through a semanticdb
;; backend. Enable it by activating `ede-php-autoload-mode'.
;;
(require 'ede)
(require 'ede-php-autoload-composer)

;;; Code:

(defvar ede-php-autoload-project-list nil
  "List of projects created by option `ede-php-autoload-project'.")

(defun ede-php-autoload-file-existing (dir)
  "Find a php-autoload project in the list of php-autoload projects.
DIR is the directory to search from."
  (let ((projs ede-php-autoload-project-list)
        (ans nil))
    (while (and projs (not ans))
      (let ((root (ede-project-root-directory (car projs))))
        (when (string-match (concat "^" (regexp-quote root)) dir)
          (setq ans (car projs))))
      (setq projs (cdr projs)))
    ans))

(defun ede-php-autoload-project-file-for-dir (&optional dir)
  "Return a full file name to the project file stored in DIR."
  (let ((proj (ede-php-autoload-file-existing dir)))
    (when proj (oref proj :file))))

;;;###autoload
(defun ede-php-autoload-project-root (&optional dir)
  "Get the root directory for DIR."
  (let ((projfile (ede-php-autoload-project-file-for-dir
                   (or dir default-directory))))
    (when projfile
      (file-name-directory projfile))))

;; Composer project detection

;;;###autoload
(defun ede-php-autoload-load (dir &optional rootproj)
  "Return a `ede-php-autoload-project' for the provided directory.

DIR is the project directory.

ROOTPROJ is the parent project.  The PHP autoload project is not
intended to be a subproject, so this argument is ignored."
  (let* ((truedir (file-truename dir))
         (name (concat "PHP Autoload: " truedir)))
    (ede-php-autoload-project name
                              :name name
                              :directory truedir
                              :file (expand-file-name ede-php-autoload-composer-file
                                                      truedir))))
;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "php-autoload"
                       :name "PHP AUTOLOAD"
                       :file 'ede-php-autoload
                       :proj-file ede-php-autoload-composer-file
                       :load-type #'ede-php-autoload-load
                       :class-sym 'ede-php-autoload-project
                       :new-p nil
                       :safe-p t)
 'unique)


;;;;
;;;; Class loaders
;;;;
;;;; In modern PHP, there is no thing like "#include" or "import".
;;;; The unknown classes are loaded at runtime using a custom loader.
;;;;
;;;; For example, with PSR-4 convention, to find the class \Bar\Foo
;;;; one have to search each include path to find the file Bar/Foo.php.

;; Helper functions

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

;; Autoloaders

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
  (error "Method `ede-php-atoload-find-class-def-file' must be overriden"))

(defun ede-php-autoload--get-path-relative-to-ns (class-name namespace)
  "Return the path of the class file relative to the namespace directory.

CLASS-NAME is the class name.

NAMESPACE is the namespace to map."
  (concat
   (mapconcat
    'identity
    (nthcdr (length (split-string namespace (rx (or "\\" "_")) t))
            (split-string class-name (rx (or "\\" "_")) t))
    "/")
   ".php"))

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

;;;###autoload
(defclass ede-php-autoload-psr0-class-loader (ede-php-autoload-class-loader)
  ((namespaces :initarg :namespaces
                  :initform ()
                  :documentation
                  "An associative list in which keys are namespaces, and  values are their include paths.

For example, if :namespaces has the value '((\"Foo\" . \"src/Foo\") (\"Bar\" . \"src/test/Bar\")),
then The class \"Bar_Foo\" is considered to be defined in \"src/test/Bar/Foo\".

The include paths can be either a string or a list of strings."))
  "Class loader for PSR-0 convention.")

(defmethod ede-php-autoload-find-class-def-file ((this ede-php-autoload-psr0-class-loader)
                                             class-name)
  "Find the file in which CLASS-NAME is defined.

Return nil if no file has been found."
  (let* ((class-name (if (= (aref class-name 0) ?\\) (substring class-name 1) class-name))
         (project-root (ede-project-root-directory (ede-current-project)))
         (namespaces (oref this namespaces))
         class-def-file)
    (while (and namespaces (not class-def-file))
      (let ((pair (car namespaces))
            (candidate-file ""))
        (when (string-prefix-p (car pair) class-name)
          (setq class-def-file (ede-php-autoload--search-in-dirs
                                (ede-php-autoload--get-path-relative-to-ns class-name (car pair))
                                (cdr pair)
                                project-root)))
        (setq namespaces (cdr namespaces))))
    class-def-file))

(defmethod ede-php-autoload-get-class-name-for-file
  ((this ede-php-autoload-psr0-class-loader) file-name)
  "Generate a suitable class name for the current FILE-NAME.

Generate this class name using the class loader information.

FILE-NAME must be absolute or relative to the project root."
  nil) ;; Work in progress

(defclass ede-php-autoload-aggregate-class-loader (ede-php-autoload-class-loader)
  ((class-loaders :initarg :class-loaders
                  :initform ()
                  :documentation "The list of aggregated class loaders.

They must be instances of `ede-php-autoload-class-loader'."))
  "An aggregation of several class loaders.")

(defmethod ede-php-autoload-find-class-def-file ((this ede-php-autoload-aggregate-class-loader)
                                             class-name)
  "Find the file in which CLASS-NAME is defined.

Return nil if no file has been found."
  (let ((loaders (oref this class-loaders))
        (class-def-file nil))
    (while (and loaders (not class-def-file))
      (setq class-def-file (ede-php-autoload-find-class-def-file (car loaders) class-name)
            loaders (cdr loaders)))
    class-def-file))

(defmethod ede-php-autoload-get-class-name-for-file
  ((this ede-php-autoload-aggregate-class-loader) file-name)
  "Generate a suitable class name for the current FILE-NAME.

Generate this class name using the class loader information.

FILE-NAME must be absolute or relative to the project root."
  (let ((loaders (oref this class-loaders))
        class-name)
    (while (and loaders (not class-name))
      (setq class-name (ede-php-autoload-get-class-name-for-file (car loaders) file-name)
            loaders (cdr loaders)))
    class-name))

(defun ede-php-autoload-create-class-loader (conf)
  "Create a class loader from a configuration.

CONF is a property list.  Its keys are class norms, and its values
are the mappings between namespace and include path.

For example, the conf '(:psr-4 ((\"Foo\" . \"src/Foo\") (\"Bar\"
\"src/test/Bar\"))) will create a class loader that will load
classes written with PSR-4 normal, mapping \"Foo\" and \"Bar\"
to the associated directories."
  (let ((loaders '())
        (load-config conf))
    (while load-config
      (let ((key (car load-config)))
        (cond
         ((equal key :psr-0)
          (add-to-list 'loaders (ede-php-autoload-psr0-class-loader "PSR-0"
                                                                :namespaces (cadr load-config))))
         ((equal key :psr-4)
          (add-to-list 'loaders (ede-php-autoload-psr4-class-loader "PSR-4"
                                                                :namespaces (cadr load-config)))))
        (setq load-config (cddr load-config))))
    (ede-php-autoload-aggregate-class-loader "Aggregate loader"
                                         :class-loaders loaders)))


(defclass ede-php-autoload-target (ede-target)
  ((project :initform nil
            :initarg :project))
  "EDE php-autoload project target.")

;;;###autoload
(defclass ede-php-autoload-project (ede-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-php-autoload-project-list)
   (class-loader :initarg :class-loader
                 :type ede-php-autoload-class-loader
                 :documentation "The project's class loader.")))

(defmethod initialize-instance ((this ede-php-autoload-project) &rest fields)
  "Make sure the :file is fully expanded."
  (let ((class-autoloads (plist-get (car fields) :class-autoloads)))

    (setq class-autoloads (ede-php-autoload--append-composer-autoload-data
                           (file-name-directory (plist-get (car fields) :file))
                           class-autoloads))

    (call-next-method this (list
                            :file (plist-get (car fields) :file)
                            :class-loader (ede-php-autoload-create-class-loader class-autoloads))))
  (let ((f (expand-file-name (oref this :file))))
    ;; Remove any previous entries from the main list.
    (let ((old (eieio-instance-tracker-find (file-name-directory f)
                                            :directory
                                            'ede-php-autoload-project-list)))
      (when (and old (not (eq old this)))
        (delete-instance old)))
    ;; Basic initialization.
    (when (or (not (file-exists-p f))
              (file-directory-p f))
      (delete-instance this)
      (error ":file for ede-php-autoload-project must be a file"))
    (oset this :file f)
    (oset this :directory (file-name-directory f))
    (ede-project-directory-remove-hash (file-name-directory f))
    (ede-add-project-to-global-list this)
    (unless (slot-boundp this 'targets)
      (oset this :targets nil))))

(defmethod ede-find-subproject-for-directory ((proj ede-php-autoload-project) dir)
  "Return PROJ, for handling all subdirs below DIR."
  proj)

(defmethod ede-find-target ((proj ede-php-autoload-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((targets (oref proj targets))
         (dir default-directory)
         (ans (object-assoc dir :path targets)))
    (when (not ans)
      (setq ans (ede-php-autoload-target dir
                                     :name (file-name-nondirectory
                                            (directory-file-name dir))
                                     :path dir
                                     :source nil
                                     :project proj))
      (object-add-to-list proj :targets ans))
    ans))

(defmethod ede-project-root ((this ede-php-autoload-project))
  "Return my root."
  this)

(defmethod ede-project-root-directory ((this ede-php-autoload-project))
  "Return my root."
  (file-name-directory (oref this file)))

(defmethod ede-php-autoload-find-class-def-file ((this ede-php-autoload-project) class-name)
  "Find the file in which CLASS-NAME is defined.

CLASS-NAME must be the full name of the class, with all its parent namespaces."
  (ede-php-autoload-find-class-def-file (oref this class-loader) class-name))

(defmethod ede-php-autoload-get-class-name-for-file
  ((this ede-php-autoload-project) file-name)
  "Generate a suitable class name for the current FILE-NAME.

Generate this class name using the class loader information.

FILE-NAME must be absolute or relative to the project root."
  (ede-php-autoload-get-class-name-for-file (oref this class-loader) file-name))

(provide 'ede-php-autoload)

;;; ede-php-autoload.el ends here
