;;; custom-modes.el --- Configuration for Modes
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
(require 'muttrc-mode)
(require 'sass-mode)
(require 'upstart-mode)
(require 'yaml-mode)
(require 'web-mode)

(defun custom-modes-map-extensions ()
  "Map file extensions to modes."
  (add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
  (add-to-list 'auto-mode-alist '("nginx.conf$" . nginx-mode))
  ;; Mac OS X .plist files
  (add-to-list 'auto-mode-alist '("\\.plist$" . xml-mode))
  ;; Files created under a /mutt dir
  (add-to-list 'auto-mode-alist '("/mutt" . mail-mode)))


(defun custom-modes-coffe-script ()
  "Coffe-script mode setup."
  (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
  (add-hook
   'coffee-mode-hook
   '(lambda() (set (make-local-variable 'tab-width) 2))))


;; Set CSS colors with themselves
(defvar custom-modes-hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :foreground
                     (match-string-no-properties 0)))))))

(defun custom-modes-css ()
  "Custom settings for CSS files when not using web-mode."
  (setq cssm-indent-function #'cssm-c-style-indenter)
  (setq cssm-indent-level 4)

  (defun hexcolour-add-to-font-lock ()
    "Configure colors for hex numbers in CSS files."
    (font-lock-add-keywords
     nil custom-modes-hexcolour-keywords))
  (add-hook 'css-mode-hook 'hexcolour-add-to-font-lock))


(defun custom-modes-diff ()
  "Diff mode configuration."
  (eval-after-load "diff-mode"
    '(lambda ()
       (set-face-attribute
        'diff-added nil :foreground "green")
       (set-face-attribute
        'diff-removed nil :foreground "red")
       (set-face-attribute
        'diff-changed nil :foreground "purple"))))


(defun custom-modes-erlang ()
  "Configuration for the Erlang mode."
  (add-hook 'erlang-mode-hook (lambda() (setq indent-tabs-mode nil)))
  (add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode)))


(defun custom-modes-html ()
  "Configuration for `html-mode'."
  (add-hook 'html-mode-hook (lambda() (setq sgml-basic-offset 4))))


(defun custom-modes-less ()
  "Less mode configuration."
  (add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
  (setq less-css-compile-at-save nil))


(defun custom-modes-markdown ()
  "Markdown mode configuration."
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-hook 'markdown-mode-hook '(lambda() (flyspell-mode))))


(defun custom-modes-sass ()
  "Custom setup for the sass-mode."
  (add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
  (add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
  (setq sass-indent-offset 4))


(defun custom-modes-term ()
  "Disable `yas' on `term-mode'."
  (add-hook
   'term-mode-hook
   '(lambda ()
      (add-hook
       'after-change-major-mode-hook
       (lambda () (yas-minor-mode 0))
       :append :local))))


(defun custom-modes-vala ()
  "Setup Vala specific configuration."
  (autoload 'vala-mode "vala-mode"
    "Major mode for editing Vala code." t)
  (add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
  (add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
  (add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
  (add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8)))


(defun custom-modes-yaml ()
  "Configuration for yaml-mode."
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  ;; Salt Stack Files
  (add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode)))


(defun custom-modes-web ()
  "Configuration for web-mode."
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-hook
   'web-mode-hook
   '(lambda ()
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-enable-current-element-highlight t)
      (setq web-mode-enable-current-column-highlight t)
      (set-face-attribute 'web-mode-doctype-face nil :foreground
                          (face-foreground font-lock-function-name-face))
      (set-face-attribute 'web-mode-html-attr-name-face nil :foreground
                          (face-foreground font-lock-variable-name-face))
      (set-face-attribute 'web-mode-html-attr-value-face nil :foreground
                          (face-foreground font-lock-type-face)))))


(defun custom-modes-prettify-symbols-alist ()
  "Use some unicode characters to prettify some symbols."
  (setq prettify-symbols-alist
        '(("lambda" . ?λ)
          ("<-" . ?⇐)
          ("->" . ?⇒)
          ("<=" . ?⤆)
          ("=>" . ?⤇)
          ("<=" . ?≤)
          (">=" . ?≥))))

(add-hook 'go-mode-hook 'custom-modes-prettify-symbols-alist)
(add-hook 'erlang-mode-hook 'custom-modes-prettify-symbols-alist)

(defun custom-modes-python ()
  "Set defaults for Python tools."
  (jedi:install-server)
  (add-hook 'python-mode-hook 'custom-modes-prettify-symbols-alist)
  (add-hook 'python-mode-hook 'jedi:setup))

(defun custom-modes ()
  "Call out all the mode setup functions."
  (custom-modes-map-extensions)
  (custom-modes-coffe-script)
  (custom-modes-css)
  (custom-modes-diff)
  (custom-modes-erlang)
  (custom-modes-html)
  (custom-modes-less)
  (custom-modes-markdown)
  (custom-modes-python)
  (custom-modes-sass)
  (custom-modes-term)
  (custom-modes-vala)
  (custom-modes-yaml)
  (custom-modes-web))

(provide 'custom-modes)
;;; custom-modes.el ends here
