;;; init.el --- My Emacs Setup
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
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
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

;;; init.el ends here
