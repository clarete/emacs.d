;;; init.el --- My Emacs Setup
;;
;;; Commentary:
;;
;; Entry point for my Emacs setup.  This module's main goal is to load
;; other files that do more specific setup work.
;;
;;; Code:

;; Default path to load lisp files
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Load packages
(require 'cask "~/.emacs.d/.cask/cask/cask.el")
(cask-initialize)

;; Load all the fun modules
(require 'custom-general)
(require 'custom-exwm-config)
(require 'custom-editing)
(require 'custom-speedbar)
(require 'custom-modes)
(require 'custom-interactive)
(require 'custom-org)

;; Initialize all the modules loaded above
(custom-general)
(custom-exwm-config)
(custom-editing)
(custom-speedbar)
(custom-modes)
(custom-org)

;; Mac specific stuff
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)

  ;; Loads environment variables from the shell
  (setq exec-path-from-shell-variables '("GOPATH" "PATH" "MANPATH"))
  (exec-path-from-shell-initialize)

  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char)
  (menu-bar-mode 1))

;;; init.el ends here
