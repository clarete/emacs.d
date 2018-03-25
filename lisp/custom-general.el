;;; custom-general.el --- General Options
;;
;;; Commentary:
;;
;; General configuration that includes setting up UI, global key
;; shortcuts, UTF-8 support, etc.
;;
;;; Code:

(require 'dired-x)
(require 'uniquify)
(require 'tramp) ;; ssh and local `sudo' and `su'
(require 'pallet)
(require 'spaceline-all-the-icons)
(require 'all-the-icons-dired)
(require 'org)

(defun custom-general-theme ()
  "Setup theme stuff."

  (load-theme 'deeper-blue t)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))

  ;; More reliable inter-window border. The native border "consumes" a
  ;; pixel of the fringe on righter-most splits
  (setq window-divider-default-places t
        window-divider-default-bottom-width 0
        window-divider-default-right-width 1)
  (window-divider-mode +1)

  ;; Necessary for org-mode
  (setq org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t))

(defun custom-general-utf-8 ()
  "Configure all known coding variables to use `UTF-8'."
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq current-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(defun custom-general-ui-fringe ()
  "Configure the Fringe area."

  ;; Custom bitmap to be shown in the fringe area for lines with any
  ;; sort of linting issues
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'my-flycheck-fringe-indicator
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00000000)))
  (flycheck-define-error-level 'error
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'my-flycheck-fringe-indicator
    :fringe-face 'flycheck-fringe-info)

  ;; Get rid of the background color in the Fringe area
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))

  ;; Finally, enable the fringe mode
  (fringe-mode 1))

(defun custom-general-ui ()
  "General UI configuration."

  ;; No bars. Doing this first to avoid showing/hidding delay on start
  ;; up
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)

  ;; Misc
  (column-number-mode)              ;; Basic config for columns
  (setq ring-bell-function 'ignore) ;; No freaking bell
  (setq inhibit-splash-screen t)    ;; No splash screen
  (setq inhibit-startup-screen t)

  ;; Some configuration for dired: icons & sort order
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (setq dired-listing-switches "-aBhl  --group-directories-first")

  ;; Omit dot files in dired by default
  (setq-default dired-omit-files-p t) ; Buffer-local variable
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

  ;; Spaceline
  (spaceline-all-the-icons-theme)
  (setq spaceline-all-the-icons-separator-type 'none)
  (spaceline-toggle-all-the-icons-time-off)
  (spaceline-toggle-all-the-icons-projectile-off)
  (spaceline-toggle-all-the-icons-buffer-path-off)
  (spaceline-toggle-all-the-icons-buffer-size-off)
  (spaceline-toggle-all-the-icons-battery-status-off)
  (spaceline-toggle-all-the-icons-hud-off))

(defun custom-general-navigation ()
  "Configuration for buffer naming."

  ;; Unique buffer names
  (setq uniquify-buffer-name-style 'reverse)

  ;; Changing the frame title to show my host name and full path of
  ;; file open on the current buffer. If `exwm' is enabled, this won't
  ;; really do anything but won't do any harm either.
  (setq frame-title-format
        (list (format "%s %%S: %%j " (system-name))
              '(buffer-file-name "%f" (dired-directory
                                       dired-directory "%b")))))

(defun custom-general-keys ()
  "Configure global key bindings."

  ;; comments
  (global-set-key [(ctrl c) (c)] 'comment-region)
  (global-set-key [(ctrl c) (d)] 'uncomment-region)

  ;; join lines
  (global-set-key [(ctrl J)] '(lambda () (interactive) (join-line -1)))

  ;; scrolling without changing the cursor
  (global-set-key [(meta n)] '(lambda () (interactive) (scroll-up 1)))
  (global-set-key [(meta p)] '(lambda () (interactive) (scroll-down 1)))

  ;; scrolling other window
  (global-set-key
   [(meta j)] '(lambda () (interactive) (scroll-other-window 1)))
  (global-set-key
   [(meta k)] '(lambda () (interactive) (scroll-other-window -1))))

(defun custom-general-misc ()
  "Miscellaneous settings and start up actions."
  (setq default-directory "~/") ;; There's no place like home
  (server-mode)

  ;; Store auto-save and backup files in a temporary directory
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  ;; Make sure `pdf-tools' is installed
  (pdf-tools-install)

  ;; Sync package list with Cask file
  (pallet-mode t)

  ;; Use screen as default shell
  (setq explicit-shell-file-name "/usr/bin/screen")

  ;; All the custom-set-variables go here
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

  ;; Set gpg binary & start Emacs pin-entry server
  (setq epg-gpg-program "gpg2")
  (setenv "INSIDE_EMACS" "YES")
  (pinentry-start))

(defun custom-general ()
  "Call out other general customization functions."
  (custom-general-ui)
  (custom-general-ui-fringe)
  (custom-general-theme)
  (custom-general-utf-8)
  (custom-general-navigation)
  (custom-general-keys)
  (custom-general-misc))

(provide 'custom-general)
;;; custom-general.el ends here
