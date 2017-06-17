;;; custom-battery.el --- General Options
;;
;;; Commentary:
;;
;; Tell me when my battery is about to die
;;
;;; Code:

(require 'battery)

(defvar custom-battery-min-level 5
  "Minimum battery percentage before starting to beep.")

(defun custom-battery-fmt (format)
  "Shortcut for battery-format FORMAT."
  (battery-format format (funcall battery-status-function)))

(defun custom-battery-check ()
  "Check battery every 5 seconds and play a sound if it's low."
  (let ((percentage (string-to-number (custom-battery-fmt "%p")))
        (status (custom-battery-fmt "%B")))
    (if (and (<= percentage custom-battery-min-level)
             (equal status "Discharging"))
        (progn
          (message "battery too low")
          (play-sound-file "~/.emacs.d/assets/sounds/beep.wav")))))

(defun custom-battery ()
  "Check battery and call itself in 5 sec."
  (custom-battery-check)
  (run-at-time "5 sec" nil #'custom-battery))

(provide 'custom-battery)
;;; custom-battery.el ends here
