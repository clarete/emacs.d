;;; lc-battery.el --- Notify when battery is running low
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
;; Tell me when my battery is about to die by ringing a bell.
;;
;; The function `lc/battery' is the entry point of this library and
;; when called, will check for battery status and then schedule itself
;; to run again after an interval.
;;
;;; Code:

(require 'battery)


(defvar lc/battery/min-level 5
  "Minimum battery percentage before starting to beep.")


(defun lc/battery/fmt (format)
  "Shortcut for battery-format FORMAT."
  (battery-format format (funcall battery-status-function)))


(defun lc/battery/check ()
  "Check battery every 5 seconds and play a sound if it's low."
  (let ((percentage (string-to-number (lc/battery/fmt "%p")))
        (status (lc/battery/fmt "%B")))
    (if (and (<= percentage lc/battery/min-level)
             (equal (downcase status) "discharging"))
        (progn
          (message "battery too low")
          (play-sound-file "~/.emacs.d/assets/sounds/beep.wav")))))


(defun lc/battery ()
  "Check battery and call itself in 5 sec."
  (lc/battery/check)
  (run-at-time "5 sec" nil #'lc/battery))

(provide 'lc-battery)
;;; lc-battery.el ends here
