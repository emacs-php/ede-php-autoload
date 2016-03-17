;;; test-helper.el --- ERT test helpers -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016, Steven Rémot

;; Author: Steven Rémot <steven.remot@gmail.com>
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
(require 'cl-lib)
;;; Code:

(global-ede-mode 1)

;; Define directory variables
(defconst test-helper--test-root (file-name-directory
                                  (or load-file-name (buffer-file-name))))

(defconst test-helper--projects-root (concat test-helper--test-root "projects/"))

(defconst test-helper--project-root (file-name-as-directory
                                     (expand-file-name ".." test-helper--test-root)))

;; Load ede-php-autoload sources
(add-to-list 'load-path test-helper--project-root)

(require 'ede-php-autoload)
(require 'ede-php-autoload-composer)
(require 'ede-php-autoload-semanticdb)
(require 'ede-php-autoload-mode)

;; Define projects
;; The composer projet is auto-detected
(ede-php-autoload-project "Without composer"
                          :file (concat test-helper--projects-root
                                        "without-composer/project")
                          :class-autoloads '(:psr-0 (("Psr0Ns" . "src/Psr0Ns")
                                                     ("Psr0Split\\Ns1" . "src/Psr0Split/Ns1")
                                                     ("Psr0Split\\Ns2" . "src/Psr0Split/Ns2"))
                                             :psr-4 (("Psr4Ns" . "src/Psr4Ns")
                                                     ("MultiDirNs" . ("src/MultiDirNs1" "src/MultiDirNs2"))
                                                     ("Psr4Split\\Ns1" . "src/Psr4Split/Ns1")
                                                     ("Psr4Split\\Ns2" . "src/Psr4Split/Ns2"))
                                             :class-map ((ClassMapNs\\MyClass . "src/ClassMapNs/MyClass.php")))
                          :include-path '(".")
                          :system-include-path '("/usr/share/php"))

;; Define function helpers
(defun test-helper-get-project-file-path (file project)
  "Return the absolute path for FILE relative to PROJECT."
  (expand-file-name file (concat test-helper--projects-root project)))

(defmacro with-current-project-file (file project &rest body)
  "Open FILE located in PROJECT and execute BODY."
  (declare (indent defun))
  `(with-current-buffer (find-file ,(test-helper-get-project-file-path file project))
     ,@body))

(cl-defmacro define-class-definition-test (name () doc &key class file-name project)
  "Test the matching of a class definition file.

NAME is the ert test name.

DOC is the test documentation.

CLASS is the name of the class to test.

If FILE-NAME is a string, it is the file path relative to the
project to test.  It it is nil, it means CLASS must not have a
definition file.

PROJECT is the project in which the test is done."
  (declare (indent defun))
  `(ert-deftest ,name ()
     ,doc
     (with-current-project-file "src/main.php" ,project
       ,(if file-name
            `(should (string=
                      ,(test-helper-get-project-file-path file-name
                                                          project)
                      (ede-php-autoload-find-class-def-file (ede-current-project)
                                                            ,class)))
          `(should-not (ede-php-autoload-find-class-def-file (ede-current-project)
                                                             ,class))))))

(provide 'test-helper)

;;; test-helper.el ends here
