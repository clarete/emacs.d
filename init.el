;;; init.el --- My Emacs Setup
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
;; Entry point for my Emacs setup.  This module's main goal is to load
;; other files that do more specific setup work.
;;
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Initialize package management system
(require 'cask (expand-file-name ".cask/cask/cask.el" user-emacs-directory))
(cask-initialize)

;; Default path to load lisp files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load all the fun modules
(require 'lc-ui)
(require 'lc-exwm)
(require 'lc-macos)
(require 'lc-general)
(require 'lc-edit)
(require 'lc-modes)
(require 'lc-org)
(require 'lc-rcirc)
(require 'lc-battery)
(require 'lc-ps)
(require 'lc-defs)
(require 'lc-vendor)

;; Initialize all the modules loaded above
(lc/ui)
(lc/exwm)
(lc/macos)
(lc/general)
(lc/edit)
(lc/modes)
(lc/org)
(lc/battery)
(lc/rcirc)
(lc/ps)
(lc/vendor)

;;; init.el ends here
