;;; lc-rcirc.el --- Home for the setup of applications
;;
;; Copyright (C) 2020  Lincoln de Sousa <lincoln@clarete.li>
;;
;; This program is free software; you can redistribute it and/or modify
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
;; Configure RCIRC to my preferences.  Currently, there are settings
;; for autojoin and authentication.
;;
;;; Code:

(require 'rcirc)
(require 'password-store)

(defun lc/rcirc/autojoin ()
  "Configure autojoin list for rcirc."
  ;; Auto join list
  (setq rcirc-server-alist
        '(("irc.freenode.net" :channels ("#emacs")))))

(defun lc/rcirc/password ()
  "Setup hook for autologin on rcirc."
  ;; Pick password from password store
  (add-hook
   'rcirc-mode-hook
   '(lambda() (set (make-local-variable 'rcirc-authinfo)
              `(("freenode"
                 nickserv
                 "lincoln"
                 ,(password-store-get "lincoln@clarete.li/freenode/pass")))))))

(defun lc/rcirc ()
  "Execute app related settings."
  (lc/rcirc/autojoin)
  (lc/rcirc/password))

(provide 'lc-rcirc)
;;; lc-rcirc.el ends here
