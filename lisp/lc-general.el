;;; custom-general.el --- General Options
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
;; General configuration that includes setting up global key
;; shortcuts, UTF-8 support, etc.
;;
;;; Code:

(require 'dired-x)
(require 'uniquify)
(require 'tramp) ;; ssh and local `sudo' and `su'

(use-package password-store)


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

  ;; Use screen as default shell
  (setq explicit-shell-file-name "/usr/bin/screen")

  ;; All the custom-set-variables go here
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

  (setq tramp-auto-save-directory "/tmp")
  (defvar disable-tramp-backups '(all))

  ;; Configure dimming of the buffers that are not active.
  (use-package dimmer
    :config
    (dimmer-mode t)
    (setq dimmer-fraction 0.5)))

(defun lc/general/autocomplete ()
  "Setup Company."
  ;; Company mode is a standard completion package that works well
  ;; with lsp-mode.
  (use-package company
    :hook (after-init . global-company-mode)
    :config
    (setq company-idle-delay .3)
    (setq company-minimum-prefix-length 10)
    (setq company-tooltip-align-annotations t)))


(defun lc/general/path ()
  "Load environment variables from the shell."
  (use-package exec-path-from-shell)
  (setq exec-path-from-shell-variables '("GOPATH" "PATH" "MANPATH"))
  (exec-path-from-shell-initialize))


(defun lc/general ()
  "Call out other general customization functions."
  (lc/general/path)
  (lc/general/utf-8)
  (lc/general/navigation)
  (lc/general/keys)
  (lc/general/misc)
  (lc/general/autocomplete))


(provide 'lc-general)
;;; lc-general.el ends here
