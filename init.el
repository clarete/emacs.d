;;; init.el --- My Emacs Setup
;;
;;; Commentary:
;;
;; Entry point for my Emacs setup.  This module's main goal is to load
;; other files that do more specific setup work.
;;
;;; Code:

(require 'cask (expand-file-name ".cask/cask/cask.el" user-emacs-directory))

;; Default path to load lisp files
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load packages
(package-initialize)
(cask-initialize)

;; Load all the fun modules
(require 'custom-general)
(require 'custom-macos)
(require 'custom-exwm-config)
(require 'custom-modes)
(require 'custom-interactive)
(require 'custom-org)
(require 'custom-auth)
(require 'custom-battery)
(require 'custom-editing)

;; Initialize all the modules loaded above
(custom-macos)
(custom-general)
(custom-exwm-config)
(custom-modes)
(custom-org)
(custom-auth)
(custom-battery)
(custom-editing)

;;; init.el ends here
