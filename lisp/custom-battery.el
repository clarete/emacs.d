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

(defun custom-battery-check ()
  "Check battery every 5 seconds and play a sound if it's low."

  (let ((percentage (string-to-number
                     (battery-format "%p" (funcall battery-status-function)))))
    (if (< percentage custom-battery-min-level)
        (progn
          (message "battery too low")
          (play-sound-file "~/.emacs.d/assets/sounds/beep.wav")))))

(defun custom-battery ()
  "."
  (custom-battery-check)
  (run-at-time "5 sec" nil #'custom-battery))

(provide 'custom-battery)
;;; custom-battery.el ends here
