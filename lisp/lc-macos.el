;;; lc-macos.el --- Specific stuff for macos
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
;; Configuration specific to macos.  There's a guard that prevents it
;; from being executed anywhere outside a darwin system.
;;
;;; Code:

;; Mac specific stuff
(defun lc/macos ()
  "Initialize stuff on macos if Emacs is running on Darwin."
  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'alt)
    (setq mac-command-modifier 'meta)

    ;; Keys for visiting next & previous windows
    (global-set-key (kbd "<A-tab>") #'other-window)
    (global-set-key (kbd "<A-S-tab>")
                    #'(lambda () (interactive) (other-window -1)))

    ;; Keys for visiting next & previous frame
    (global-set-key (kbd "M-`") #'other-frame)
    (global-set-key (kbd "M-~") #'(lambda () (interactive) (other-frame -1)))

    ;; sets fn-delete to be right-delete
    (global-set-key [kp-delete] 'delete-char)
    (menu-bar-mode 1)))

(provide 'lc-macos)
;;; lc-macos.el ends here
