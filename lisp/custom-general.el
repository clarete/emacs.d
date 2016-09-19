;;; custom-general.el --- General Options
;;
;;; Commentary:
;;
;; General configuration that includes setting up UI, global key
;; shortcuts, UTF-8 support, etc.
;;
;;; Code:

(require 'uniquify)
(require 'tramp) ;; ssh and local sudo/su


(defun custom-general-utf-8 ()
  "Configure all known coding variables to use UTF-8."
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq current-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))


(defun custom-general-ui ()
  "General UI configuration."

  ;; No bars. Doing this first to avoid showing/hidding delay on
  ;; startup
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (fringe-mode 0)

  (load-theme 'deeper-blue) ;; Theme
  (column-number-mode)      ;; Basic config for columns

  (setq ring-bell-function 'ignore) ;; No freaking bell
  (setq inhibit-splash-screen t)    ;; No splash screen
  (setq inhibit-startup-screen t))


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

  ;; Store autosave and backup files in a temporary directory
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))


(defun custom-general ()
  "Call out other general customization functions."
  (custom-general-ui)
  (custom-general-utf-8)
  (custom-general-navigation)
  (custom-general-keys)
  (custom-general-misc))


(provide 'custom-general)
;;; custom-general.el ends here
