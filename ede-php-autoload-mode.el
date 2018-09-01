;;; ede-php-autoload-mode.el --- Minor mode for activating ede-php-autoload tag loading

;; Copyright (C) 2015, Steven Rémot

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
;;; Code:

(require 'semantic/db)
(require 'ede-php-autoload-semanticdb)


(defgroup ede-php-autoload nil ""
  :group 'tools)

;;;###autoload
(define-minor-mode ede-php-autoload-mode
  "ede-php-autoload // Enable PHP tag loading using `ede-php-autoload-project'.

\\{ede-php-autoload-mode-map}"
  :group ede-php-autoload

  (cond (ede-php-autoload-mode
            (unless (listp semanticdb-project-system-databases)
              (setq semanticdb-project-system-databases '()))
            (add-to-list 'semanticdb-project-system-databases (ede-php-autoload-semanticdb-database "EDE PHP ROOT")))

           ;; On mode disable, remove ede-php-autoload database
           (t (let (new-databases '())
                (dolist (database semanticdb-project-system-databases)
                  (unless (ede-php-autoload-semanticdb-database-p database)
                    (push database new-databases)))
                (setq semanticdb-project-system-databases new-databases)))))

(provide 'ede-php-autoload-mode)

;;; ede-php-autoload-mode.el ends here
