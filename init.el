;;; init.el --- My Emacs Setup
;;;
;;; Commentary:
;;; This is my local Emacs setup
;;;
;;; Code:

;; Default path to load lisp files
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'defuns)

;; Session init
(shell-command "xsetroot -default && xmodmap ~/.Xmodmap")

;; No bars. Doing this first to avoid showing/hidding delay on startup
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Initialize cask
(require 'cask "~/.emacs.d/.cask/cask/cask.el")
(cask-initialize)

;; X window system integration
;; https://github.com/ch11ng/exwm/wiki
(require 'exwm)
(require 'custom-exwm-config)
(custom-exwm-config)

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

;; Font configuration
(global-font-lock-mode 1)               ;; Always do syntax highlighting
(transient-mark-mode 1)                 ;; Highlight mark region
(set-frame-font "Monospace 12" t t)     ;; Font face/size
(global-prettify-symbols-mode 1)        ;; See prettify-symbols-alist

;; Show Line numbers
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")

;; Do not wrap lines
(setq-default truncate-lines t)

;;; Also highlight parenthesis
(show-paren-mode 1)

;;; Store autosave and backup files in a temporary directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Editing options
(setq-default indent-tabs-mode nil)    ;; spaces instead of tabs
(setq scroll-conservatively 10000)     ;; scroll smoothly
(setq show-trailing-whitespace t)      ;; Whitespaces
(setq x-select-enable-clipboard t)     ;; Clipboard shared with the DE

;;; Other small configurations
(setq default-directory "~/")          ;; There's no place like home

;;; Org mode
(setq org-agenda-files '("~/Org"))

;; speedbar
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag nil)
(setq speedbar-auto-refresh nil)
(setq speedbar-frame-parameters
      '((minibuffer)
	(width . 40)
	(border-width . 0)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(unsplittable . t)
	(left-fringe . 0)))
(add-hook 'go-mode-hook
          (lambda ()
            (speedbar-add-supported-extension ".go")
            (setq imenu-generic-expression
                  '((nil "^type *\\([^ \t\n\r\f]*\\)" 1)
                    (nil "^func *\\(.*\\) {" 1)
                    (nil "^def *\\([^\(]\\):" 1)
                    (nil "^class *\\([^\(]\\):" 1))
                  imenu-case-fold-search nil
                  speedbar-tag-hierarchy-method nil)
            (imenu-add-to-menubar "Index")))
(global-set-key [(ctrl c) (ctrl tab)] 'sr-speedbar-toggle)

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


;; upstart mode
(require 'upstart-mode "~/.emacs.d/3rdparty/upstart-mode.el")

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

;; erlang config
(add-hook 'erlang-mode-hook (lambda() (setq indent-tabs-mode nil)))

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
                      (face-foreground font-lock-type-face)))
(add-hook 'web-mode-hook  'web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
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
(defun coffee-custom ()
  "Set tab width for coffe script files."
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
(yas-load-directory "~/.emacs.d/snippets")
(yas-global-mode 1)

;; Configuring the dropdown list, submodule used by yasnippet
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt))

;; Esk search!
;;(add-to-list 'load-path "~/.emacs.d/elisp/esk")
;;(require 'esk)
;;(global-set-key "\M-s" 'esk-find-file)
;;(global-set-key "\M-\S-s" 'esk-find-in-project)
;;(setq esk-find-binary "gfind")

;; Some more on prettifying chars
(defun set-prettify-symbols-alist ()
  "Use some unicode characters to prettify some symbols."
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
(add-hook 'erlang-mode-hook 'set-prettify-symbols-alist)

;; Customizing colors used in diff mode
(defun custom-diff-colors ()
  "Update the colors for diff faces."
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

;; This disables both linum-mode & yas-minor-mode when in term-mode
(add-hook 'term-mode-hook 'my-term-mode-setup)
(defun my-term-mode-setup ()
  "Disable `linum' & `yas' on `term-mode'."
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (linum-mode 0)
              (yas-minor-mode 0))
            :append :local))

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Enable syntax checks
(global-flycheck-mode)

;; Enabling the server mode by default
(server-mode)

;;; init.el ends here
