;;; classmap.el --- Class map class loader implementation

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
;; Class loader based on a strong mapping between class names and files.
;;
;; To use it :
;;    (ede-php-autoload-project "My project"
;;                              :file "main.php"
;;                              :class-autoloads '(:class-map (("Class\\Name" . "file/name.php")
;;                                                            ("Class\\Name2" . "file/name2.php"))))
;;
;;; Code:
(require 'ede)
(require 'ede-php-autoload/class-loader/core)

;;;###autoload
(defclass ede-php-autoload-classmap-class-loader (ede-php-autoload-class-loader)
  ((class-hash :initarg :classes
               :initform (makehash)
               :documentation "A hash associating a class name and an absolute path to the file in whcih the class is defined."))
  "Class loader for direct association between classes and files.")

(defmethod ede-php-autoload-find-class-def-file ((this ede-php-autoload-classmap-class-loader) class-name)
  "Find the file in which CLASS-NAME is defined.

Return nil if no file has been found."
  (let ((file (gethash class-name (oref this class-hash))))
    (when file
      (expand-file-name file (ede-project-root-directory (ede-current-project))))))

(defmethod ede-php-autoload-get-class-name-for-file ((this ede-php-autoload-classmap-class-loader) file-name)
  "Generate a suitable class name for the current FILE-NAME.

Generate this class name using the class loader information.

FILE-NAME must be absolute or relative to the project root."
  (let* ((project-root (ede-project-root (ede-current-project)))
         (abs-file-name (expand-file-name file-name project-root)))
    (catch 'class-name
      (maphash
       #'(lambda (class file)
           (when (string= (expand-file-name file project-root) abs-file-name)
             (throw 'class-name class)))
       (oref this class-hash)))))

(defmethod ede-php-autoload-complete-type-name ((this ede-php-autoload-classmap-class-loader) prefix)
  "Get completion suggestions for the type PREFIX.

PREFIX is the beginning of a fully-qualified name.

The result is a list of completion suggestions for this
prefix."
  (let ((completions '()))
    (maphash
     #'(lambda (class file)
         (when (string-prefix-p prefix class)
           (add-to-list 'completions class)))
     (oref this class-hash))
    completions))

(ede-php-autoload-class-loader-define-factory :class-map (classes)
  (let ((class-hash))

    ;; Convert association lists to hash map
    (if (listp classes)
        (progn
          (setq class-hash (make-hash-table :test 'equal))
          (dolist (pair classes)
            (puthash (format "%s" (car pair)) (cdr pair) class-hash)))
      (setq class-hash classes))

    (ede-php-autoload-classmap-class-loader "Classmap" :classes class-hash)))

(provide 'ede-php-autoload/class-loader/classmap)

;;; classmap.el ends here
