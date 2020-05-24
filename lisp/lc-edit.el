;;; lc-edit.el --- Editing Options
;;
;;; Commentary:
;;
;; Editing setup includes Fonts, Line Numbers, Tabs vs Spaces, Auto
;; Complete, Code Snippets, etc.
;;
;;; Code:

(require 'linum)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'yasnippet)
(require 'flycheck)
(require 'smartparens)


(defun lc/edit/fonts ()
  "Setup font details."
  (global-font-lock-mode 1)           ;; Always do syntax highlighting
  (transient-mark-mode 1)             ;; Highlight mark region
  (global-prettify-symbols-mode 1)    ;; See prettify-symbols-alist
  (let ((myfont "FantasqueSansMono")) ;; Font face
    (set-frame-font myfont t t)
    (set-face-attribute 'default nil
                        :family myfont
                        :height 120
                        :weight 'normal
                        :width 'normal)))


(defun lc/edit/line-numbers ()
  "Configure line numbers in the Emacs UI."
  (add-hook 'conf-mode-hook 'linum-on)
  (add-hook 'prog-mode-hook 'linum-on)
  (add-hook 'text-mode-hook 'linum-on)
  (setq linum-format "%d "))


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
  (lc/edit/fonts)
  (lc/edit/line-numbers)
  (lc/edit/auto-complete)
  (lc/edit/code-snippets)
  (lc/edit/misc)
  (lc/edit/multiple-cursors))


(provide 'lc-edit)
;;; lc-edit.el ends here
