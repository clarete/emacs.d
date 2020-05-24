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
(require 'all-the-icons-dired)
(require 'dimmer)


(defun lc/general/utf-8 ()
  "Configure all known coding variables to use `UTF-8'."
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq current-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))


(defun lc/general/navigation ()
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


(defun lc/general/keys ()
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


(defun lc/general/misc ()
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

  (setq tramp-auto-save-directory "/tmp")
  (defvar disable-tramp-backups '(all))

  ;; Configure dimming of the buffers that are not active.
  (dimmer-mode t)
  (setq dimmer-fraction 0.5))


(defun lc/general ()
  "Call out other general customization functions."
  (lc/general/utf-8)
  (lc/general/navigation)
  (lc/general/keys)
  (lc/general/misc))


(provide 'lc-general)
;;; lc-general.el ends here
