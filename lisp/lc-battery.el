;;; custom-battery.el --- General Options
;;
;;; Commentary:
;;
;; Tell me when my battery is about to die
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
             (equal status "Discharging"))
        (progn
          (message "battery too low")
          (play-sound-file "~/.emacs.d/assets/sounds/beep.wav")))))

(defun lc/battery ()
  "Check battery and call itself in 5 sec."
  (lc/battery/check)
  (run-at-time "5 sec" nil #'custom-battery))

(provide 'lc-battery)
;;; lc-battery.el ends here
