;;; custom-org.el --- Setup for org-mode
;;; Commentary:
;;
;; Copyright (C) 2016  Lincoln de Sousa <lincoln@comum.org>
;;
;;; Code:

(require 'org)
(require 'org-agenda)


(defun custom-org-directory-dirs (dir)
  "List directories recursively inside of DIR."
  (remove-if
   #'(lambda (p) (not (file-directory-p p)))
   (let* ((dir dir)
          (dirs (directory-files
                 dir t directory-files-no-dot-files-regexp)))
     (push (expand-file-name dir) dirs))))


(defun custom-org ()
  "Configuration for the `org-mode'."
  (setq org-agenda-files (custom-org-directory-dirs "~/Org"))
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t))


(provide 'custom-org)
;;; custom-org.el ends here
