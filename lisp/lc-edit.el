;;; lc-edit.el --- Editing Options
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
;; Editing setup includes setting up line Numbers, Tabs vs Spaces,
;; Auto Complete, Code Snippets, etc.
;;
;;; Code:

(require 'linum)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'yasnippet)
(require 'flycheck)
(require 'smartparens)


(defun lc/edit/line-numbers ()
  "Configure line numbers in the Emacs UI."
  (add-hook 'conf-mode-hook 'linum-on)
  (add-hook 'prog-mode-hook 'linum-on)
  (add-hook 'text-mode-hook 'linum-on)
  (setq linum-format 'dynamic))


(defun lc/edit/auto-complete ()
  "Enable and Configure the auto-complete feature."
  (global-auto-complete-mode t)
  (setq ac-dwim 2)
  (ac-config-default)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous))


(defun lc/edit/code-snippets ()
  "Configuration for yasnippets."
  (yas-load-directory "~/.emacs.d/snippets")
  (yas-global-mode 1))


(defun lc/edit/misc ()
  "Misc editing settings."

  ;; Enable smart parens everywhere
  (smartparens-global-mode)

  ;; Do not wrap lines
  (setq-default truncate-lines t)

  ;; spaces instead of tabs
  (setq-default indent-tabs-mode nil)

  ;; Complain about trailing white spaces
  (setq show-trailing-whitespace t)

  ;; Also highlight parenthesis
  (show-paren-mode 1)

  ;; scroll smoothly
  (setq scroll-conservatively 10000)

  ;; Enable syntax checks
  (global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))

  ;; Find peace with syntax checking when requiring files located in
  ;; custom paths
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Clipboard shared with the Desktop Environment. I wonder if the
  ;; `exwm' integration would work without this line.
  (setq select-enable-clipboard t))


(defun lc/edit/multiple-cursors()
  "Setup multiple-cursor."
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))


(defun lc/edit ()
  "Call out other editing customization functions."
  (lc/edit/line-numbers)
  (lc/edit/auto-complete)
  (lc/edit/code-snippets)
  (lc/edit/misc)
  (lc/edit/multiple-cursors))


(provide 'lc-edit)
;;; lc-edit.el ends here
