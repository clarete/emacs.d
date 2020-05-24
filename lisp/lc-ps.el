;;; custom-services.el --- General Options
;;
;;; Commentary:
;;
;; Setup background services with prodigy
;;
;;; Code:


(defun lc/ps/define ()
  "Define processes ran by prodigy."
  (prodigy-define-service
    :name "offlineimap"
    :command "~/bin/check-email"
    :cwd "~"))


(defun lc/ps ()
  "Entry point for process list."
  (lc/ps/define))

(provide 'lc-ps)
;;; lc-ps.el ends here
