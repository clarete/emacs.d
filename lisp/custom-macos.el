;;; custom-macos.el --- Specific stuff for macos
;;
;;; Commentary:
;;
;; General configuration that includes setting up UI, global key
;; shortcuts, UTF-8 support, etc.
;;
;;; Code:

;; Mac specific stuff
(defun custom-macos ()
  "Initialize stuff on macos if Emacs is running on darwin."
  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'alt)
    (setq mac-command-modifier 'meta)

    ;; Loads environment variables from the shell
    (setq exec-path-from-shell-variables '("GOPATH" "PATH" "MANPATH"))
    (exec-path-from-shell-initialize)

    ;; sets fn-delete to be right-delete
    (global-set-key [kp-delete] 'delete-char)
    (menu-bar-mode 1)))

(provide 'custom-macos)
;;; custom-macos.el ends here
