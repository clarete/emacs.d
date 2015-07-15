;; No bars. Doing this first to avoid showing/hidding delay on startup
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Initialize cask
(require 'cask "~/.emacs.d/.cask/cask/cask.el")
(cask-initialize)

;; utf-8 for good (is there any other encoding related var I could set?)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq current-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; UI Configuration
(load-theme 'deeper-blue)               ;; Theme
(column-number-mode)                    ;; Basic config for columns

(setq ring-bell-function 'ignore)       ;; No freaking bell
(setq inhibit-splash-screen t)          ;; No splash screen
(setq inhibit-startup-screen t)

(global-font-lock-mode 1)               ;; Always do syntax highlighting
(transient-mark-mode 1)                 ;; highlight mark region
(set-default-font "Monaco 12")          ;; Font face/size

(global-prettify-symbols-mode 1)        ;; See prettify-symbols-alist

(require 'linum)                       ;; show line numbers
(global-linum-mode 1)

(setq-default truncate-lines t)        ;; Do not wrap lines

;;; Also highlight parenthesis
(setq show-paren-delay 0 show-paren-style 'parenthesis)
(show-paren-mode 1)

;;; Editing options
(setq-default indent-tabs-mode nil)    ;; spaces instead of tabs
(setq make-backup-files nil)           ;; No backup files
(setq scroll-conservatively 10000)     ;; scroll smoothly
(setq show-trailing-whitespace t)      ;; Whitespaces
(setq x-select-enable-clipboard t)     ;; Clipboard shared with the DE

;;; Other small configurations
(setq gdb-many-windows 1)              ;; gdb
(setq default-directory "~/")          ;; There's no place like home

;; speedbar
(require 'sr-speedbar)
(add-hook 'go-mode-hook
          (lambda ()
            (speedbar-add-supported-extension ".go")
            (setq imenu-generic-expression
                  '((nil "^type *\\([^ \t\n\r\f]*\\)" 1)
                    (nil "^func *\\(.*\\) {" 1))
                  imenu-case-fold-search nil
                  speedbar-tag-hierarchy-method nil)
            (imenu-add-to-menubar "Index")))
(global-set-key [(ctrl c) (ctrl tab)] 'sr-speedbar-toggle)

;; Reloading the buffer instead of pissing me off with "what should I
;; do" questions
(defun ask-user-about-supersession-threat (filename)
  ;; (revert-buffer t t)
  (message "This buffer was refreshed due to external changes"))

;; ---- key bindings ---

;; comments
(global-set-key [(ctrl c) (c)] 'comment-region)
(global-set-key [(ctrl c) (d)] 'uncomment-region)

;; join lines
(global-set-key [(ctrl J)] '(lambda () (interactive) (join-line -1)))

;; moving from one window to another
(global-set-key [(ctrl <)] 'next-multiframe-window)
(global-set-key [(ctrl >)] 'previous-multiframe-window)

;; moving from one frame to another
(global-set-key [(C-tab)] 'other-window)
(global-set-key [(shift C-tab)] '(lambda () (interactive) (other-window -1)))

;; scrolling without changing the cursor
(global-set-key [(meta n)] '(lambda () (interactive) (scroll-up 1)))
(global-set-key [(meta p)] '(lambda () (interactive) (scroll-down 1)))

;; scrolling other window
(global-set-key [(meta j)] '(lambda () (interactive) (scroll-other-window 1)))
(global-set-key [(meta k)] '(lambda () (interactive) (scroll-other-window -1)))

;; resize windows
(global-set-key (kbd "C-}") 'shrink-window-horizontally)
(global-set-key (kbd "C-{") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-}") 'shrink-window)
(global-set-key (kbd "C-M-{") 'enlarge-window)

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

(require 'tramp)           ;; ssh and local sudo/su
(require 'muttrc-mode)     ;; mutt and muttrc modes
(require 'lua-mode)        ;; lua mode

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode)) ; salt stack

;;; Map file extensions to modes
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))        ;; Vagrant
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))              ;; PHP
(add-to-list 'auto-mode-alist '("nginx.conf$" . nginx-mode))        ;; nginx
(add-to-list 'auto-mode-alist '("\\.plist$" . xml-mode))            ;; Mac OS X .plist files
(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))           ;; erlang
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))               ;; Files created under a /mutt dir
(add-hook 'html-mode-hook (lambda() (setq sgml-basic-offset 4)))    ;; html mode

;; css config
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 4)

; Set CSS colors with themselves
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :foreground
                     (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)

;; JSON
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; sass mode
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(setq sass-indent-offset 4)

;; less mode
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))
(setq less-compile-at-save nil)

;; Web mode
(require 'web-mode)
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (set-face-attribute 'web-mode-doctype-face nil :foreground
                      (face-foreground font-lock-function-name-face))
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground
                      (face-foreground font-lock-variable-name-face))
  (set-face-attribute 'web-mode-html-attr-value-face nil :foreground
                      (face-foreground font-lock-type-face)))
(add-hook 'web-mode-hook  'web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Markdown mode
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook '(lambda() (flyspell-mode)))

;; Vala mode
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;; CoffeScript mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom () "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

;; Earl gray mode
;; (add-to-list 'load-path "~/.emacs.d/elisp/earl-grey-mode")
;; (require 'earl-mode)
;; (add-to-list 'auto-mode-alist '("\\.eg$" . earl-mode))

;; Auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-dwim 2)
(ac-config-default)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; Loading YAS personal snippets
(require 'yasnippet)
(setq yas-root-directory "~/.emacs.d/snippets")
(yas-global-mode 1)
(yas-load-directory yas-root-directory)

;; Configuring the dropdown list, submodule used by yasnippet
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt))

;; Esk search!
;;(add-to-list 'load-path "~/.emacs.d/elisp/esk")
;;(require 'esk)
;;(global-set-key "\M-s" 'esk-find-file)
;;(global-set-key "\M-\S-s" 'esk-find-in-project)
;;(setq esk-find-binary "gfind")

;; Some git shortcuts
(defun git () (interactive) (magit-status "."))
(defun git-blame () (interactive) (mo-git-blame-current))

;; Some more on prettifying chars
(defun set-prettify-symbols-alist ()
    (setq prettify-symbols-alist
          '(
            ("lambda" . ?λ)
            ("<-" . ?⤆)
            ("->" . ?⤇)
            ("<=" . ?⇐)
            ("=>" . ?⇒)
            ("<=" . ?≤)
            (">=" . ?≥)
            )))
(add-hook 'python-mode-hook 'set-prettify-symbols-alist)
(add-hook 'go-mode-hook 'set-prettify-symbols-alist)

;; Pyflakes stuff
(require 'flymake-cursor)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "flake8"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
;; (setq flymake-gui-warnings-enabled nil)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; Customizing colors used in diff mode
(defun custom-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute
   'diff-added nil :foreground "green")
  (set-face-attribute
   'diff-removed nil :foreground "red")
  (set-face-attribute
   'diff-changed nil :foreground "purple"))
(eval-after-load "diff-mode" '(custom-diff-colors))

;; Changing the frame title to show my host name and full path of file open on
;; the current buffer
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Loading some custom functions after loading everything else
(load "~/.emacs.d/defuns.el")

;; Enabling the server mode by default
(server-mode)
