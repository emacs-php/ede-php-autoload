;;; ede-php-autoload-test.el --- ERT tests for ede-php-autoload-project -*- lexical-binding: t -*-

;; Copyright (C) 2015, Steven Rémot

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

;; Without composer
(ert-deftest ede-php-autoload-wt-composer-project-is-defined ()
  "The EDE php autoload project should be defined."
  (with-current-project-file "main.php" "without-composer"
    (should (ede-php-autoload-project-p (ede-current-project)))))

(define-class-definition-test ede-php-autoload-wt-composer-find-psr0 ()
  "The definition for a PSR-4 class should be found."
  :class "Psr0Ns_TheClass"
  :file-name "src/Psr0Ns/TheClass.php"
  :project "without-composer")

(define-class-definition-test ede-php-autoload-wt-composer-find-psr4 ()
  "The definition for a PSR-4 class should be found."
  :class "Psr4Ns\\TheClass"
  :file-name "src/Psr4Ns/TheClass.php"
  :project "without-composer")

(define-class-definition-test ede-php-autoload-wt-composer-find-nothing ()
  "nil should be returned when no definition have been found."
  :class "Psr4Ns\\DoesNotExist"
  :file-name nil
  :project "without-composer")

;; With composer
(ert-deftest ede-php-autoload-composer-project-is-defined ()
  "The EDE php autoload project should be defined."
  (with-current-project-file "main.php" "with-composer"
    (should (ede-php-autoload-project-p (ede-current-project)))))

(define-class-definition-test ede-php-autoload-composer-find-psr0 ()
  "The definition for a PSR-4 class should be found."
  :class "Psr0Ns_TheClass"
  :file-name "src/Psr0Ns/TheClass.php"
  :project "with-composer")

(define-class-definition-test ede-php-autoload-composer-find-psr4 ()
  "The definition for a PSR-4 class should be found."
  :class "Psr4Ns\\TheClass"
  :file-name "src/Psr4Ns/TheClass.php"
  :project "with-composer")

(define-class-definition-test ede-php-autoload-composer-find-nothing ()
  "nil should be returned when no definition have been found."
  :class "Psr4Ns\\DoesNotExist"
  :file-name nil
  :project "with-composer")

(define-class-definition-test ede-php-autoload-composer-find-third-party ()
  "The definition for a composer dependency's class should be found."
  :class "ThirdParty\\ThirdClass"
  :file-name "vendor/third-party/third-party/src/ThirdClass.php"
  :project "with-composer")

(provide 'ede-php-autoload-test)

;;; ede-php-autoload-test.el ends here
