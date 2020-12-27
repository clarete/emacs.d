;;; custom-services.el --- General Options
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
;; Setup background services with prodigy.
;;
;;; Code:


(defun lc/ps/define ()
  "Define processes ran by prodigy."
  (use-package prodigy
    :config
    (prodigy-define-service
     :name "offlineimap"
     :command "~/bin/check-email"
     :cwd "~")))


(defun lc/ps ()
  "Entry point for process list."
  (lc/ps/define))

(provide 'lc-ps)
;;; lc-ps.el ends here
