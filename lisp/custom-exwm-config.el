;;; custom-exwm-config.el --- EXWM configuration
;;; Commentary:
;;
;; Personalized configuration for EXWM
;;
;;; Code:

(require 'exwm)

(defun custom-exwm-config ()
  "Custom configuration of EXWM."

  ;; Don't ask if a new buffer should be created. Just do it!
  (setq async-shell-command-buffer 'new-buffer)

  ;; Don't show buffer with output for asynchronous commands
  (add-to-list
   'display-buffer-alist
   '(".*Async Shell Command.*" (display-buffer-no-window)))

  ;; Set the initial workspace number.
  (setq exwm-workspace-number 4)

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; 's-r': Reset
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)

  ;; 's-w': Switch workspace
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

  ;; 's-N': Switch to certain workspace
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))

  ;; 's-&': Launch application
  (exwm-input-set-key (kbd "s-&")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))

  ;; Line-editing shortcuts
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))))

  ;; Enable EXWM
  (exwm-enable))

(provide 'custom-exwm-config)
;;; custom-exwm-config ends here
