;;; lc-exwm.el --- EXWM configuration
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
;; Configuration for EXWM.  Ranging from keyboard shortcuts to dual
;; monitor setup.
;;
;;; Code:

(use-package exwm)
(require 'exwm)
(require 'exwm-randr)
(require 'exwm-systemtray)

(defun lc/exwm/global-keys ()
  "Configure Keybindings for exwm."

  ;; Bind window cycling to Meta-Tab & Meta-Shift-Tab
  (exwm-input-set-key (kbd "<M-tab>") #'other-window)
  (exwm-input-set-key
   (kbd "<M-iso-lefttab>")
   #'(lambda () (interactive) (other-window -1)))

  ;; Unbind <M-tab> when in magit
  (with-eval-after-load 'magit-mode
    (define-key magit-mode-map (kbd "<M-tab>") nil))
  ;; Unbind <M-tab> in eshell
  (add-hook 'eshell-mode-hook
            (lambda () (define-key eshell-mode-map (kbd "<M-tab>") nil)))

  ;; Bind keys for resizing windows
  (exwm-input-set-key (kbd "C-{") #'shrink-window-horizontally)
  (exwm-input-set-key (kbd "C-}") #'enlarge-window-horizontally)
  (exwm-input-set-key (kbd "C-M-}") 'shrink-window)
  (exwm-input-set-key (kbd "C-M-{") 'enlarge-window)

  ;; 's-r': Reset
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)

  ;; 's-w': Switch workspace
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

  ;; 's-N': Switch to certain workspace
  (dotimes (i exwm-workspace-number)
    (exwm-input-set-key (kbd (format "s-%d" (1+ i)))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))

  ;; 's-&': Launch application
  (exwm-input-set-key (kbd "s-&")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command))))


(defun lc/exwm/editing-keys ()
  "Configure Line-editing shortcuts for X11 apps."
  (setq exwm-input-simulation-keys
        '(([?\C-b] . left)
          ([?\C-f] . right)
          ([?\C-p] . up)
          ([?\C-n] . down)
          ([?\C-a] . home)
          ([?\C-e] . end)
          ([?\M-v] . prior)
          ([?\C-v] . next)
          ([?\C-d] . delete)
          ([?\C-y] . 22)                     ; Paste
          ([?\M-w] . 3)                      ; Copy
          ([?\C-k] . (S-end 24)))))          ; Select til the end & Cut


(defun lc/exwm/shell-command ()
  "Configure Async Shell Command buffer options."

  ;; Don't ask if a new buffer should be created. Just do it!
  (setq async-shell-command-buffer 'new-buffer)

  ;; Don't show buffer with output for asynchronous commands
  (add-to-list
   'display-buffer-alist
   '(".*Async Shell Command.*" (display-buffer-no-window))))


(defun lc/exwm/buffer-name ()
  "Rename buffer according to WM_CLASS and WM_NAME (or _NET_WM_NAME)."
  (exwm-workspace-rename-buffer (concat exwm-class-name " - " exwm-title)))


(defun lc/exwm/wm-options ()
  "Set misc Window Manager options."

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook #'lc/exwm/buffer-name)
  (add-hook 'exwm-update-title-hook #'lc/exwm/buffer-name)

  ;; Set the initial workspace number.
  (setq exwm-workspace-number 6))


(defun lc/exwm/x11-helpers ()
  "Setup Desktop Background & X11 Key mapping."
  (shell-command "xsetroot -default && xmodmap ~/.Xmodmap")
  (shell-command "xbindkeys"))


(defun lc/exwm/randr ()
  "Setup xrandr defaults."
  ;; Workspace 0 is locked in the laptop screen (eDP-1). All the other
  ;; workspaces go to external monitor (DP-1) when connected.
  (setq exwm-randr-workspace-monitor-plist
        (let ((workspaces '(0 "eDP-1")))
          (dotimes (i (- exwm-workspace-number 1))
            (setq workspaces (append workspaces `(,(+ 1 i) "DP-1"))))
          workspaces))
  ;; External monitor is on the left side of the laptop screen
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output DP-1 --left-of eDP-1 --auto")))
  (exwm-randr-enable))


(defun lc/exwm ()
  "Custom configuration of EXWM."
  (when (not (eq system-type 'darwin))
    (lc/exwm/x11-helpers)
    (lc/exwm/wm-options)
    (lc/exwm/shell-command)
    (lc/exwm/global-keys)
    (lc/exwm/editing-keys)
    (lc/exwm/randr)
    (exwm-systemtray-enable)
    (exwm-enable)))


(provide 'lc-exwm)
;;; lc-exwm ends here
