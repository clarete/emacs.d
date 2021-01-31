;;; lc-modes.el --- Configuration for Modes
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
;; Configuration for different modes, like markdown, vala, css, etc.
;;
;;; Code:

(use-package magit)
(use-package vterm)
(use-package ox-reveal)
(use-package package-lint)

(require 'css-mode)
(require 'dired-x)

(defun lc/modes/lsp ()
  "Configure LSP mode."
  (use-package lsp-mode
    :commands (lsp lsp-deferred)
    :hook (go-mode . lsp-deferred))
  (use-package lsp-ui
    :commands lsp-ui-mode
    :config (setq
             lsp-ui-doc-enable nil
             lsp-ui-peek-enable t
             lsp-ui-sideline-enable t
             lsp-ui-imenu-enable t
             lsp-ui-flycheck-enable t)))

(defun lc/modes/map-extensions ()
  "Map file extensions to modes."
  (add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
  (add-to-list 'auto-mode-alist '("nginx.conf$" . nginx-mode))
  ;; Mac OS X .plist files
  (add-to-list 'auto-mode-alist '("\\.plist$" . xml-mode)))


(defun lc/modes/pug ()
  "Configuration for pug-mode."
  (use-package pug-mode
    :config
    (setq pug-tab-width 2)
    :hook (pug-mode . (lambda()
			(set (make-local-variable 'tab-width) 2)))))


;; Set CSS colors with themselves
(defvar lc/modes/hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :foreground
                     (match-string-no-properties 0)))))))

(defun lc/modes/css ()
  "Custom settings for CSS files when not using web-mode."

  (defun hexcolour-add-to-font-lock ()
    "Configure colors for hex numbers in CSS files."
    (font-lock-add-keywords nil lc/modes/hexcolour-keywords))
  (add-hook 'css-mode-hook 'hexcolour-add-to-font-lock))


(defun lc/modes/diff ()
  "Diff mode configuration."
  (eval-after-load "diff-mode"
    '(lambda ()
       (set-face-attribute
        'diff-added nil :foreground "green")
       (set-face-attribute
        'diff-removed nil :foreground "red")
       (set-face-attribute
        'diff-changed nil :foreground "purple"))))


(defun lc/modes/erlang ()
  "Configuration for the Erlang mode."
  (add-hook 'erlang-mode-hook (lambda() (setq indent-tabs-mode nil)))
  (add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode)))


(defun lc/modes/html ()
  "Configuration for `html-mode'."
  (add-hook 'html-mode-hook (lambda() (setq sgml-basic-offset 2))))


(defun lc/modes/less ()
  "Less mode configuration."
  (use-package less-css-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
    (setq less-css-compile-at-save nil)))


(defun lc/modes/markdown ()
  "Markdown mode configuration."
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (use-package markdown-mode
    :hook (markdown-mode . flyspell-mode)
    :config
    (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))))


(defun lc/modes/sass ()
  "Custom setup for the sass-mode."
  (use-package sass-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
    (add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
    (setq sass-indent-offset 4)))


(defun lc/modes/term ()
  "Disable `yas' on `term-mode'."
  (add-hook
   'term-mode-hook
   '(lambda ()
      (add-hook
       'after-change-major-mode-hook
       (lambda () (yas-minor-mode 0))
       :append :local))))


(defun lc/modes/go ()
  "Install and configure Go mode."
  (use-package go-mode)
  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(defun lc/modes/vala ()
  "Setup Vala specific configuration."
  (autoload 'vala-mode "vala-mode"
    "Major mode for editing Vala code." t)
  (add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
  (add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
  (add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
  (add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8)))


(defun lc/modes/yaml ()
  "Configuration for yaml-mode."
  (use-package yaml-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
    ;; Salt Stack Files
    (add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))))


(defun lc/modes/web ()
  "Configuration for web-mode."
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (setq js-indent-level 2)
  (setq typescript-indent-level 2)

  (use-package js2-mode
    :config
    (setq js2-basic-offset 2)
    (setq js2-strict-trailing-comma-warning nil)
    (setq js2-strict-missing-semi-warning nil)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

  (use-package web-mode
    :config
    (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
    (setq web-mode-enable-auto-indentation nil)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-current-column-highlight t)
    :custom-face
    (web-mode-doctype-face
     ((t (:foreground ,(face-foreground font-lock-function-name-face)))))
    (web-mode-html-attr-name-face
     ((t (:foreground ,(face-foreground font-lock-variable-name-face)))))
    (web-mode-html-attr-value-face
     ((t (:foreground ,(face-foreground font-lock-type-face)))))))


(defun lc/modes/pdf-tools ()
  "Set default for PDF mode."
  (use-package pdf-tools
    :init #'pdf-tools-install
    :hook (pdf-view-mode . (lambda () (pdf-view-midnight-minor-mode)))))

(defun lc/modes/python ()
  "Set defaults for Python tools."
  (use-package lsp-python-ms
    :ensure t
    :init (setq lsp-python-ms-auto-install-server t)
    :hook (python-mode . (lambda ()
                           (require 'lsp-python-ms)
                           (lsp))))  ; or lsp-deferred
  )

(defun lc/modes/lua ()
  "Set defaults for Lua code."
  (use-package lua-mode
    :config
    (setq lua-indent-level 2)))

(defun lc/modes/rust ()
  "Set defaults for Rust code."
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(defun lc/modes/protobuf ()
  "Setup `protobuf-mode'."
  (use-package protobuf-mode
    :hook (protobuf-mode . (lambda ()
                             ;; extend CC mode with my config
                             (c-add-style "protobuf"
                                          '((c-basic-offset . 4)
                                            (indent-tabs-mode . nil)))
                             (c-set-style "protobuf")
                             ;; enable line numbers
                             (display-line-numbers-mode)))))

;; Configuration for dired to remember omit hidden state.  Found this
;; piece of code at https://www.emacswiki.org/emacs/DiredOmitMode
;; signed by the user `kuanyui`.  Thank you!

(defvar lc/modes/v-dired-omit t
  "If dired-omit-mode enabled by default (don't setq me).")


(defun lc/modes/dired-omit-switch ()
  "Enhancement for `dired-omit-mode', to \"remember\" omit state across Dired buffers."
  (interactive)
  (if (eq lc/modes/v-dired-omit t)
      (setq lc/modes/v-dired-omit nil)
    (setq lc/modes/v-dired-omit t))
  (lc/modes/dired-omit-caller)
  (revert-buffer))


(defun lc/modes/dired-omit-caller ()
  "The entry point for the switch."
  (if lc/modes/v-dired-omit
      (setq dired-omit-mode t)
    (setq dired-omit-mode nil)))


(defun lc/modes/dired ()
  "Configuration for dired mode."

  ;; Some configuration for dired: icons & sort order
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (setq dired-listing-switches "-aBhl  --group-directories-first")

  ;; Omit dot files in dired by default
  (setq-default dired-omit-files-p t) ; Buffer-local variable
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

  ;; Rember to keep hiden files hiden
  (define-key dired-mode-map (kbd "C-x M-o") 'lc/modes/dired-omit-switch)
  (add-hook 'dired-mode-hook 'lc/modes/dired-omit-caller))


(defun lc/modes/prog ()
  "General configuration for `prog-mode'."
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)

  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode)))

;; https://lists.gnu.org/archive/html/help-gnu-emacs/2007-05/msg00975.html
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))


(defun lc/modes ()
  "Call out all the mode setup functions."
  (lc/modes/lsp)
  (lc/modes/prog)
  (lc/modes/dired)
  (lc/modes/lua)
  (lc/modes/map-extensions)
  (lc/modes/pug)
  (lc/modes/css)
  (lc/modes/diff)
  (lc/modes/erlang)
  (lc/modes/html)
  (lc/modes/less)
  (lc/modes/markdown)
  (lc/modes/pdf-tools)
  (lc/modes/go)
  (lc/modes/python)
  (lc/modes/rust)
  (lc/modes/sass)
  (lc/modes/term)
  (lc/modes/vala)
  (lc/modes/yaml)
  (lc/modes/web)
  (lc/modes/protobuf))


(provide 'lc-modes)
;;; lc-modes.el ends here
