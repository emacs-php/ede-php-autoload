;;; ede-php-autoload-composer-test.el --- Unit test for ede-php-autoload-composer

;;; Commentary:
;;
;; Copyright (C) 2017, Steven Rémot

;; Author: Steven Rémot <steven.remot@gmail.com>
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

;;; Code:

(ert-deftest ede-php-autoload-composer-merge-autoloads-concats-files ()
  "`ede-php-autoload-composer-merge-autoloads' should concat provided autoloads."
  (should (equal

           (ede-php-autoload-composer-merge-autoloads
            '(:psr-0 (("Test" . "test/")) :psr-4 (("Test3" . "test3/")))
            '(:psr-0 (("Test2" . "test2/")) :psr-4 (("Test4" . "test4/"))))

           '(:psr-0 (("Test" . "test/")
                     ("Test2" . "test2/"))
             :psr-4 (("Test3" . "test3/")
                     ("Test4" . "test4/"))))))

(ert-deftest ede-php-autoload-composer-define-visitor ()
  "`:define-visitors' should define a visitor according to its step."
  (setq ede-php-autoload-composer--visitors '())

  (ede-php-autoload-composer-define-visitor 'test-visitor-1)
  (should (equal ede-php-autoload-composer--visitors '((:normal . (test-visitor-1)))))

  (ede-php-autoload-composer-define-visitor 'test-visitor-2 :late)
  (should (equal ede-php-autoload-composer--visitors '((:late . (test-visitor-2))
                              (:normal . (test-visitor-1)))))


  (ede-php-autoload-composer-define-visitor 'test-visitor-3 :early)
  (should (equal ede-php-autoload-composer--visitors '((:early . (test-visitor-3))
                              (:late . (test-visitor-2))
                              (:normal . (test-visitor-1)))))


  (ede-php-autoload-composer-define-visitor 'test-visitor-4)
  (should (equal ede-php-autoload-composer--visitors '((:early . (test-visitor-3))
                              (:late . (test-visitor-2))
                              (:normal . (test-visitor-4 test-visitor-1))))))

(ert-deftest ede-php-autoload-composer--run-visitors ()
  "`ede-php-autoload-composer--run-visitors' should build configuration from visitors."
  (should (equal
           (ede-php-autoload-composer--run-visitors
            '((:late . ((lambda (context autoloads)
                          (push :late autoloads))))
              (:early . ((lambda (context autoloads)
                           (push :early autoloads))))
              (:normal . ((lambda (context autoloads)
                          (push :normal autoloads)))))
            nil
            '())

           '(:late :normal :early))))

(provide 'ede-php-autoload-composer-test)

;;; ede-php-autoload-composer-test.el ends here
