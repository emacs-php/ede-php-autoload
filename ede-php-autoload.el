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
(require 'json)

;;; Code:

(defvar ede-php-autoload-project-list nil
  "List of projects created by otpion `ede-php-autoload-project'.")

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

(defun ede-php-autoload-load (dir &optional rootproj)
  "Return a PHP root object if you created one.
Return nil if there isn't one.
DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (ede-php-autoload-file-existing dir))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "php-autoload"
                       :name "PHP AUTOLOAD"
                       :file 'ede-php-autoload
                       :proj-file 'ede-php-autoload-project-file-for-dir
                       :proj-root 'ede-php-autoload-project-root
                       :load-type 'ede-php-autoload-load
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
  (let* ((namelist (split-string class-name (regexp-quote "\\") t))
         (relative-path (concat (mapconcat 'identity (cdr namelist) "/") ".php"))
         (project-root (ede-project-root-directory (ede-current-project)))
         (namespaces (oref this namespaces))
         class-def-file)
    (while (and namespaces (not class-def-file))
      (let ((pair (car namespaces)))
        (when (string= (car namelist) (car pair))
          (setq class-def-file (ede-php-autoload--search-in-dirs relative-path
                                                                 (cdr pair)
                                                                 project-root)))
        (setq namespaces (cdr namespaces))))
    class-def-file))

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
         (namelist (split-string class-name (rx (or "\\" "_")) t))
         (relative-path (concat (mapconcat 'identity (cdr namelist) "/") ".php"))
         (project-root (ede-project-root-directory (ede-current-project)))
         (namespaces (oref this namespaces))
         class-def-file)
    (while (and namespaces (not class-def-file))
      (let ((pair (car namespaces))
            (candidate-file ""))
        (when (string= (car namelist) (car pair))
          (setq class-def-file (ede-php-autoload--search-in-dirs relative-path
                                                                 (cdr pair)
                                                                 project-root)))
        (setq namespaces (cdr namespaces))))
    class-def-file))

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

;;; Composer support
(defun ede-php-autoload--format-composer-single-dir (namespace path base-dir standard)
  "Format a composer autoload pair when the path is a single string.

NAMESPACE is the autoloaded namespace.

PATH is a string representing the relative directory.

BASE-DIR is the directory PATH is relative to.

STANDARD is the autoload standard (e.g. `psr-0')."

    (when (stringp base-dir)
      (setq path (concat (file-name-as-directory base-dir) path)))

    (when (string= "psr-0" standard)
      (setq path (concat path namespace "/")))

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

(defun ede-php-autoload--get-composer-data (dir)
  "Return the parsed composer.json file in DIR if any.

Return nil otherwise."
  (let ((composer-file (expand-file-name "composer.json" dir)))
    (when (file-exists-p composer-file)
      (json-read-file composer-file))))

(defun ede-php-autoload--get-composer-third-party-dirs (composer-data)
  "Get all composer project's third party lib dirs.

COMPOSER-DATA is the composer.json data to scan."
  (let ((vendor-dir (file-name-as-directory
                     (or (assoc-default 'vendor-dir composer-data) "vendor")))
        (third-parties (assoc-default 'require composer-data '()))
        (dirs '()))
    (dolist (third-party third-parties)
      (add-to-list 'dirs (concat vendor-dir (symbol-name (car third-party)))))
    dirs))

(defun ede-php-autoload--append-composer-autoload-data (project-dir autoloads)
  "Add all composer autoload information.

If PROJECT-DIR has a composer specification, add its autoload
information into AUTOLOADS."
  (let* ((root-data (ede-php-autoload--get-composer-data project-dir))
         (third-party-dirs (ede-php-autoload--get-composer-third-party-dirs root-data)))
    (setq autoloads (ede-php-autoload--merge-composer-autoloads root-data autoloads nil))

    (dolist (dir third-party-dirs)
      (setq autoloads (ede-php-autoload--merge-composer-autoloads
                       (ede-php-autoload--get-composer-data
                        (expand-file-name dir project-dir))
                       autoloads
                       dir)))
    autoloads))

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

(provide 'ede-php-autoload)

;;; ede-php-autoload.el ends here
