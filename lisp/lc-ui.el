;;; lc-ui.el --- Configuration for Emacs UI
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
;; Configure UI aspects of Emacs, like fonts, menu bars, the look and
;; feel of the modeline etc.
;;
;;; Code:

(require 'org)
(require 'doom-modeline)

(defun lc/ui/general ()
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
  (setq inhibit-startup-screen t))


(defun lc/ui/theme ()
  "Setup theme stuff."

  (load-theme 'gruvbox t)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
  (set-face-attribute 'linum nil
                      :foreground (face-foreground 'font-lock-comment-face)
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


(defun lc/ui/fonts ()
  "Setup font details."
  (global-font-lock-mode 1)           ;; Always do syntax highlighting
  (transient-mark-mode 1)             ;; Highlight mark region
  (global-prettify-symbols-mode 1)    ;; See prettify-symbols-alist
  (let ((myfont "Fantasque Sans Mono")) ;; Font face
    (set-frame-font myfont t t)
    (set-face-attribute 'default nil
                        :family myfont
                        :height 120
                        :weight 'normal
                        :width 'normal)))

(defun lc/ui/fringe ()
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
  (fringe-mode 15))


(defun lc/ui/modeline ()
  "Configuration for the modeline."
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 1)
  (doom-modeline-mode 1))


(defun lc/ui ()
  "Entry point of UI configuration."
  (lc/ui/general)
  (lc/ui/theme)
  (lc/ui/fonts)
  (lc/ui/fringe)
  (lc/ui/modeline))


(provide 'lc-ui)
;;; lc-ui.el ends here
