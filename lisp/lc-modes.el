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

(require 'coffee-mode)
(require 'css-mode)
(require 'less-css-mode)
(require 'lua-mode)
(require 'markdown-mode)
(require 'sass-mode)
(require 'yaml-mode)
(require 'web-mode)
(require 'pug-mode)
(require 'pdf-view)
(require 'js2-mode)
(require 'prettier-js)
(require 'rainbow-delimiters)
(require 'dired-x)

(defun lc/modes/map-extensions ()
  "Map file extensions to modes."
  (add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
  (add-to-list 'auto-mode-alist '("nginx.conf$" . nginx-mode))
  ;; Mac OS X .plist files
  (add-to-list 'auto-mode-alist '("\\.plist$" . xml-mode)))

(defun lc/modes/coffe-script ()
  "Coffe-script mode setup."
  (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
  (add-hook
   'coffee-mode-hook
   '(lambda() (set (make-local-variable 'tab-width) 2))))


(defun lc/modes/pug ()
  "Configuration for pug-mode."
  (setq pug-tab-width 2)
  (add-hook
   'pug-mode-hook
   '(lambda() (set (make-local-variable 'tab-width) 2))))


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
    (font-lock-add-keywords
     nil lc/modes/hexcolour-keywords))
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
  (add-hook 'html-mode-hook (lambda() (setq sgml-basic-offset 4))))


(defun lc/modes/less ()
  "Less mode configuration."
  (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
  (setq less-css-compile-at-save nil))


(defun lc/modes/markdown ()
  "Markdown mode configuration."
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-hook 'markdown-mode-hook '(lambda() (flyspell-mode))))


(defun lc/modes/sass ()
  "Custom setup for the sass-mode."
  (add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
  (add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
  (setq sass-indent-offset 4))


(defun lc/modes/term ()
  "Disable `yas' on `term-mode'."
  (add-hook
   'term-mode-hook
   '(lambda ()
      (add-hook
       'after-change-major-mode-hook
       (lambda () (yas-minor-mode 0))
       :append :local))))


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
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  ;; Salt Stack Files
  (add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode)))


(defun lc/modes/web ()
  "Configuration for web-mode."
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-enable-auto-indentation nil)
  (setq js2-basic-offset 2)
  (setq js-indent-level 2)
  (setq typescript-indent-level 2)
  (setq js2-strict-trailing-comma-warning nil)
  (setq js2-strict-missing-semi-warning nil)

  (add-hook
   'web-mode-hook
   '(lambda ()
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-enable-current-element-highlight t)
      (setq web-mode-enable-current-column-highlight t)
      ;(prettier-js-mode)
      (set-face-attribute 'web-mode-doctype-face nil :foreground
                          (face-foreground font-lock-function-name-face))
      (set-face-attribute 'web-mode-html-attr-name-face nil :foreground
                          (face-foreground font-lock-variable-name-face))
      (set-face-attribute 'web-mode-html-attr-value-face nil :foreground
                          (face-foreground font-lock-type-face)))))

(defun lc/modes/prettify-symbols-alist ()
  "Use some unicode characters to prettify some symbols."
  ;; (setq prettify-symbols-alist
  ;;       '(("lambda" . ?λ)
  ;;         ("<-" . ?⇐)
  ;;         ("->" . ?⇒)
  ;;         ("<=" . ?⤆)
  ;;         ("=>" . ?⤇)
  ;;         ("<=" . ?≤)
  ;;         (">=" . ?≥)))
  )

(add-hook 'go-mode-hook 'lc/modes/prettify-symbols-alist)
(add-hook 'erlang-mode-hook 'lc/modes/prettify-symbols-alist)
(add-hook 'scala-mode-hook 'lc/modes/prettify-symbols-alist)
;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(defun lc/modes/pdf-tools ()
  "Set default for PDF mode."
  (add-hook 'pdf-view-mode-hook
            (lambda () (pdf-view-midnight-minor-mode))))

(defun lc/modes/python ()
  "Set defaults for Python tools."
  (jedi:install-server)
  (add-hook 'python-mode-hook 'lc/modes/prettify-symbols-alist)
  (add-hook 'python-mode-hook 'jedi:setup))

(defun lc/modes/lua ()
  "Set defaults for Lua code."
  (setq lua-indent-level 2))

(defun lc/modes/rust ()
  "Set defaults for Rust code."
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

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
  (message "fuuu")
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


;; https://lists.gnu.org/archive/html/help-gnu-emacs/2007-05/msg00975.html
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))


(defun lc/modes ()
  "Call out all the mode setup functions."
  (lc/modes/dired)
  (lc/modes/lua)
  (lc/modes/map-extensions)
  (lc/modes/coffe-script)
  (lc/modes/pug)
  (lc/modes/css)
  (lc/modes/diff)
  (lc/modes/erlang)
  (lc/modes/html)
  (lc/modes/less)
  (lc/modes/markdown)
  (lc/modes/pdf-tools)
  (lc/modes/python)
  (lc/modes/rust)
  (lc/modes/sass)
  (lc/modes/term)
  (lc/modes/vala)
  (lc/modes/yaml)
  (lc/modes/web))


(provide 'lc-modes)
;;; lc-modes.el ends here
