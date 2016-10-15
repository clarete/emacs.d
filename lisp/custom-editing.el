;;; custom-editing.el --- Editing Options
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
(require 'dropdown-list)                ; yasnippet dependency


(defun custom-editing-fonts ()
  "Setup font details."
  (global-font-lock-mode 1)           ;; Always do syntax highlighting
  (transient-mark-mode 1)             ;; Highlight mark region
  (set-frame-font "Monospace 12" t t) ;; Font face/size
  (global-prettify-symbols-mode 1))   ;; See prettify-symbols-alist


(defun custom-editing-line-numbers ()
  "Configure line numbers in the Emacs UI."
  (add-hook 'conf-mode-hook 'linum-on)
  (add-hook 'prog-mode-hook 'linum-on)
  (add-hook 'text-mode-hook 'linum-on)
  (setq linum-format "%d "))


(defun custom-editing-auto-complete ()
  "Enable and Configure the auto-complete feature."
  (global-auto-complete-mode t)
  (setq ac-dwim 2)
  (ac-config-default)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous))


(defun custom-editing-code-snippets ()
  "Configuration for yasnippets."
  (setq yas-prompt-functions '(yas-dropdown-prompt))
  (yas-load-directory "~/.emacs.d/snippets")
  (yas-global-mode 1))


(defun custom-editing-misc ()
  "Misc editing settings."

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

  ;; Clipboard shared with the Desktop Environment. I wonder if the
  ;; `exwm' integration would work without this line.
  (setq x-select-enable-clipboard t))


(defun custom-editing ()
  "Call out other editing customization functions."
  (custom-editing-fonts)
  (custom-editing-line-numbers)
  (custom-editing-auto-complete)
  (custom-editing-code-snippets)
  (custom-editing-misc))


(provide 'custom-editing)
;;; custom-editing.el ends here
