;;; class-loader.el --- Include all autoloading code

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
;;; Code:
(require 'ede-php-autoload/class-loader/core)
(require 'ede-php-autoload/class-loader/psr4)
(require 'ede-php-autoload/class-loader/psr0)
(require 'ede-php-autoload/class-loader/classmap)
(require 'ede-php-autoload/class-loader/aggregate)

(provide 'ede-php-autoload/class-loader)

;;; class-loader.el ends here
