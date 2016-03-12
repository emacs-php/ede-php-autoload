;;; aggregate.el --- Class loader composed of several other loaders

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

(defmethod ede-php-autoload-complete-type-name ((this ede-php-autoload-aggregate-class-loader) prefix)
  "Get completion suggestions for the type PREFIX.

PREFIX is the beginning of a fully-qualified name.

The result is a list of completion suggestions for this
prefix. Completions are not guaranteed to give full class names,
this can only suggest the next namespace."
  (let ((suggestions '()))
    (dolist (loader (oref this class-loaders))
      (setq suggestions (append suggestions
                                (ede-php-autoload-complete-type-name loader prefix))))
    suggestions))

(provide 'ede-php-autoload/class-loader/aggregate)

;;; aggregate.el ends here
