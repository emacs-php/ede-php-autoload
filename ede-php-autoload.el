;;; ede-php-autoload.el --- Simple EDE PHP Project

;; Copyright (C) 2014, 2015, 2016, Steven Rémot

;; Author: Steven Rémot <steven.remot@gmail.com>
;;         original code for C++ by Eric M. Ludlam <eric@siege-engine.com>
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
;; PHP EDE project that supports class autoloading and composer.json detection.
;;
;; Example project definition :
;; (ede-php-autoload-project "My project"
;;                       :file "/path/to/a/file/at/root"
;;                       :class-autoloads '(:psr-0 (("MyNs" . "src/MyNs")
;;                                                 ("AnotherNs" . "src/AnotherNs"))
;;                                          :psr-4 (("MyModernNs" . "src/modern/MyNs"))))
;;
;; This EDE project can then be used through a semanticdb
;; backend.  Enable it by activating `ede-php-autoload-mode'.
;;

(require 'ede)

(require 'ede-php-autoload-composer)
(require 'ede-php-autoload/class-loader)

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
    (when proj (oref proj file))))

(defun ede-php-autoload--proj-root (file)
  "Recursively detect project root.

Return the top-most parent directory of FILE containing a composer.json file."
  (let* ((dominating-file (locate-dominating-file file ede-php-autoload-composer-file))
         potential-proj-root)
    (when dominating-file
      (setq potential-proj-root (file-name-directory dominating-file))
      (or (ede-php-autoload--proj-root (file-name-directory (directory-file-name potential-proj-root))) potential-proj-root))))

;;;###autoload
(defun ede-php-autoload-proj-root ()
  "Auto-detect composer project root.

Return the parent directory of the current buffer file that contains a composer.json file."
  (ede-php-autoload--proj-root (or (buffer-file-name) default-directory)))

;; Composer project detection

;;;###autoload
(defun ede-php-autoload-load (dir &optional rootproj)
  "Return a `ede-php-autoload-project' for the provided directory.

DIR is the project directory.

ROOTPROJ is the parent project.  The PHP autoload project is not
intended to be a subproject, so this argument is ignored."
  (let* ((truedir (ede-php-autoload--proj-root (file-truename dir)))
         (name (concat "PHP Autoload: " truedir)))
    (ede-php-autoload-project name
                              :name name
                              :directory truedir
                              :file (expand-file-name ede-php-autoload-composer-file
                                                      truedir))))

;;;###autoload
(eval-after-load 'ede
  '(ede-add-project-autoload
     (ede-project-autoload "php-autoload"
                           :name "PHP AUTOLOAD"
                           :file 'ede-php-autoload
                           :proj-file "composer.json"
                           :proj-root 'ede-php-autoload-proj-root
                           :load-type 'ede-php-autoload-load
                           :class-sym 'ede-php-autoload-project
                           :new-p nil
                           :safe-p t)
     'unique))

(defun ede-php-autoload-reload-autoloads ()
  "Reload the autoloads for the current projects.

This has the same goal than a reindexation in IDEs.  Use this
method when your composer.json file changed, or your vendor
directory has been updated in order to take the new autoloads
into account."
  (interactive)
  (ede-php-autoload-reload-autoloads-for-project (ede-current-project)))

;;;;
;;;; Class loaders
;;;;

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
        (add-to-list 'loaders (ede-php-autoload-class-loader-call-factory key (cadr load-config)))
        (setq load-config (cddr load-config))))
    (ede-php-autoload-aggregate-class-loader "Aggregate loader"
                             :class-loaders loaders)))

(defun ede-php-autoload-remove-non-existing-dirs (conf root-dir)
  "Remove from CONF the non-existing directories.

CONF is the same kind of argument than `ede-php-autoload-create-class-loader'.

ROOT-DIR is the root directory of the project."
  (let ((cleaned-conf '())
        (conf-length (length conf))
        (index 0)
        key
        namespaces cleaned-namespaces
        namespace
        paths cleaned-paths)

    ;; key: `:psr-0', ...
    (while (< index conf-length)
      (setq key (nth index conf)
            namespaces (nth (1+ index) conf)
            cleaned-namespaces '())

      ;; pair: (ns . paths)
      (dolist (pair namespaces)
        (setq namespace (car pair)
              paths (if (listp (cdr pair)) (cdr pair) (list (cdr pair)))
              cleaned-paths '())

        (dolist (path paths)
          (when (file-exists-p (expand-file-name path root-dir))
            (add-to-list 'cleaned-paths path t)))

        (when (> (length cleaned-paths) 0)
          (add-to-list 'cleaned-namespaces
                       (cons
                        namespace
                        (if (= (length cleaned-paths) 1)
                            (car cleaned-paths)
                          cleaned-paths))
                       t)))

      (when (> (length cleaned-namespaces) 0)
        (setq cleaned-conf (append cleaned-conf (list key cleaned-namespaces))))

      (setq index (+ index 2)))
    cleaned-conf))


(defclass ede-php-autoload-target (ede-target)
  ((project :initform nil
            :initarg :project))
  "EDE php-autoload project target.")

;;;###autoload
(defclass ede-php-autoload-project (ede-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-php-autoload-project-list)
   (class-loader :type ede-php-autoload-class-loader
                 :documentation "The project's class loader.")
   (include-path :initarg :include-path
                 :type list
                 :initform ()
                 :documentation "A list of PHP include paths specific to the project")
   (system-include-path :initarg :system-include-path
                        :type list
                        :initform ()
                        :documentation "The list of PHP include paths defined for the system.")
   (explicit-class-autoloads :initarg :explicit-class-autoloads
                             :type list
                             :documentation "The class autoloads explicitly defined at initialization")))

(defmethod initialize-instance ((this ede-php-autoload-project) &rest fields)
  "Make sure the :file is fully expanded."
  (call-next-method this (list
                          :file (plist-get (car fields) :file)
                          :explicit-class-autoloads (plist-get (car fields) :class-autoloads)
                          :include-path (plist-get (car fields) :include-path)
                          :system-include-path (plist-get (car fields) :system-include-path)))

  (ede-php-autoload-reload-autoloads-for-project this)

  (let ((f (expand-file-name (oref this file))))
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

(defmethod ede-php-autoload-reload-autoloads-for-project ((this ede-php-autoload-project))
  "Regenerate the class loaders.

This can be used when some composer dependencies changed, to take
the new autoloads into account."
  (let* ((raw-autoloads
          (ede-php-autoload--append-composer-autoload-data
           (file-name-directory (ede-project-root-directory this))
           (oref this explicit-class-autoloads)))
         (root-dir (ede-project-root-directory this))
         (cleaned-autoloads (ede-php-autoload-remove-non-existing-dirs raw-autoloads root-dir)))

    (oset this class-loader
          (ede-php-autoload-create-class-loader cleaned-autoloads))))

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

(defmethod ede-php-autoload-complete ((this ede-php-autoload-project) prefix)
  "Get completion suggestions for the type PREFIX.

PREFIX is the beginning of a fully-qualified name.

The result is a list of completion suggestions for this
prefix."
  (ede-php-autoload-complete (oref this class-loader) prefix))

(defmethod ede-php-autoload-complete-type-name ((this ede-php-autoload-project) prefix)
  "Get completion suggestions for the type PREFIX.

PREFIX is the beginning of a fully-qualified name.

The result is a list of completion suggestions for this
prefix. Completions are not guaranteed to give full class names,
this can only suggest the next namespace."
  (ede-php-autoload-complete-type-name (oref this class-loader) prefix))

(provide 'ede-php-autoload)

;;; ede-php-autoload.el ends here
