;;; ede-php-autoload-test.el --- ERT tests for ede-php-autoload-project -*- lexical-binding: t -*-

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

;;; Code:
(ert-deftest ede-php-autoload-project-is-defined ()
  "The EDE php autoload project should be defined."
  (with-current-project-file "src/main.php" "without-composer"
    (should (ede-php-autoload-project-p (ede-current-project)))))

(ert-deftest ede-php-autoload-project-has-include-path ()
  "The include path defined in the project is readable."
  (with-current-project-file "src/main.php" "without-composer"
    (should (string= (car (oref (ede-current-project) :include-path)) "."))))

(ert-deftest ede-php-autoload-project-has-system-include-path ()
  "The system include path defined in the project is readable."
  (with-current-project-file "src/main.php" "without-composer"
    (should (string= (car (oref (ede-current-project) :system-include-path))
                     "/usr/share/php"))))

;; Class loading tests
(define-class-definition-test ede-php-autoload-find-psr0 ()
  "The definition for a PSR-4 class should be found."
  :class "Psr0Ns_TheClass"
  :file-name "src/Psr0Ns/TheClass.php"
  :project "without-composer")

(define-class-definition-test ede-php-autoload-find-psr4 ()
  "The definition for a PSR-4 class should be found."
  :class "Psr4Ns\\TheClass"
  :file-name "src/Psr4Ns/TheClass.php"
  :project "without-composer")

(define-class-definition-test ede-php-autoload-find-multidir-1 ()
  "The definition of the first item of a multi-directory namespace should be found."
  :class "MultiDirNs\\TheClass1"
  :file-name "src/MultiDirNs1/TheClass1.php"
  :project "without-composer")

(define-class-definition-test ede-php-autoload-find-multidir-2 ()
  "The definition of the non-first item of a multi-directory namespace should be found."
  :class "MultiDirNs\\TheClass2"
  :file-name "src/MultiDirNs2/TheClass2.php"
  :project "without-composer")

(define-class-definition-test ede-php-autoload-find-psr0-split-without-clash ()
  "The definition of a class in a split namespace (iwht multiple sub-namespaces) should be found on PSR-0."
  :class "Psr0Split\\Ns2\\TheClass"
  :file-name "src/Psr0Split/Ns2/TheClass.php"
  :project "without-composer")

(define-class-definition-test ede-php-autoload-find-psr4-split-without-clash ()
  "The definition of a class in a split namespace (iwht multiple sub-namespaces) should be found on PSR-4."
  :class "Psr4Split\\Ns2\\TheClass"
  :file-name "src/Psr4Split/Ns2/TheClass.php"
  :project "without-composer")

(define-class-definition-test ede-php-autoload-find-nothing ()
  "nil should be returned when no definition have been found."
  :class "Psr4Ns\\DoesNotExist"
  :file-name nil
  :project "without-composer")

(define-class-definition-test ede-php-autoload-find-classmap ()
  "The definition of a class referred in a classmap should be found."
  :class "ClassMapNs\\MyClass"
  :file-name "src/ClassMapNs/MyClass.php"
  :project "without-composer")

(ert-deftest ede-php-autoload-guess-class-name-psr4 ()
  "A class definition can be guessed by looking PSR4 autoloading configuration."
  (with-current-project-file "src/main.php" "without-composer"
    (should (string= (ede-php-autoload-get-class-name-for-file
                      (ede-current-project)
                      "src/MultiDirNs1/SubNs1/SubNs2/Class.php")
                     "MultiDirNs\\SubNs1\\SubNs2\\Class"))))

(ert-deftest ede-php-autoload-guess-class-name-psr4-split-ns ()
  "A class definition can be guessed by looking PSR4 autoloading configuration.

In this tests, the base namespace is split."
  (with-current-project-file "src/main.php" "without-composer"
    (should (string= (ede-php-autoload-get-class-name-for-file
                      (ede-current-project)
                      "src/Psr4Split/Ns2/MyClass.php")
                     "Psr4Split\\Ns2\\MyClass"))))

(ert-deftest ede-php-autoload-guess-class-name-for-classmap ()
  "A class definition can be guessed by looking at classmap autoloading configuration."
  (with-current-project-file "src/main.php" "without-composer"
    (should (string= (ede-php-autoload-get-class-name-for-file
                      (ede-current-project)
                      "src/ClassMapNs/MyClass.php")
                     "ClassMapNs\\MyClass"))))

;; Type completion tests

(ert-deftest ede-php-autoload-complete-psr-0-namespace-base ()
  "`ede-php-autoload-complete-type' should complete PSR-0 prefixes."
  (with-current-project-file "src/main.php" "without-composer"
    (should (equal (ede-php-autoload-complete-type-name (ede-current-project) "Psr0")
                   '("Psr0Ns" "Psr0Split\\Ns1" "Psr0Split\\Ns2")))))

(ert-deftest ede-php-autoload-complete-psr-0-with-slashes ()
  "`ede-php-autoload-complete-type' should complete PSR-0 with
slashes when detected."
  (with-current-project-file "src/main.php" "without-composer"
    (should (equal (ede-php-autoload-complete-type-name (ede-current-project) "Psr0Ns\\T")
                   '("TheClass")))))

(ert-deftest ede-php-autoload-complete-psr-0-with-underscores ()
  "`ede-php-autoload-complete-type' should complete PSR-0 with
underscores when detected."
  (with-current-project-file "src/main.php" "without-composer"
    (should (equal (ede-php-autoload-complete-type-name (ede-current-project) "Psr0Ns_T")
                   '("Psr0Ns_TheClass")))))

(ert-deftest ede-php-autoload-complete-psr-4-namespace-base ()
  "`ede-php-autoload-complete-type' should complete PSR-4 prefixes."
  (with-current-project-file "src/main.php" "without-composer"
    (should (equal (ede-php-autoload-complete-type-name (ede-current-project) "Psr4")
                   '("Psr4Ns" "Psr4Split\\Ns1" "Psr4Split\\Ns2")))))

(ert-deftest ede-php-autoload-complete-psr-4-with-one-dir ()
  "`ede-php-autoload-complete-type' should complete for PSR-4
 namespace one directory."
  (with-current-project-file "src/main.php" "without-composer"
    (should (equal (ede-php-autoload-complete-type-name (ede-current-project)
                                                        "Psr4Ns\\T")
                   '("TheClass" )))))

(ert-deftest ede-php-autoload-complete-psr-4-with-multiple-dirs ()
  "`ede-php-autoload-complete-type' should complete for PSR-4
 namespace with multiple directories."
  (with-current-project-file "src/main.php" "without-composer"
    (should (equal (ede-php-autoload-complete-type-name (ede-current-project)
                                                        "MultiDirNs\\T")
                   '("TheClass1" "TheClass2")))))

(ert-deftest ede-php-autoload-complete-class-map ()
  "`ede-php-autoload-complete-type' should complete for classmap namespaces."
  (with-current-project-file "src/main.php" "without-composer"
    (should (equal (ede-php-autoload-complete-type-name (ede-current-project)
                                                        "ClassMapNs\\")
                   '("ClassMapNs\\MyClass")))))

(provide 'ede-php-autoload-test)

;;; ede-php-autoload-test.el ends here
