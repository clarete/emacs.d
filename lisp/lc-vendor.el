;;; lc-vendor.el --- Load and configure site-lisp packages
;;
;; Author: Lincoln Clarete <lincoln@clarete.li>
;;
;; Copyright (C) 2012-2020  Lincoln Clarete
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Load and configure packages that are present within the site-lisp
;; directory.
;;
;;; Code:


(defun lc/vendor/path ()
  "Insert `site-lisp' into `load-path'."
  (add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory)))


(defun lc/vendor/require ()
  "Load vendorized dependencies."
  (require 'peg-mode))


(defun lc/vendor ()
  "Entry point for configuration of site-lisp."
  (lc/vendor/path)
  (lc/vendor/require))


(provide 'lc-vendor)
;;; lc-vendor.el ends here
