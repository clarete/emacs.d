;;; lc-org.el --- Setup for org-mode
;;
;;
;;; Commentary:
;;
;; Copyright (C) 2016-2020  Lincoln de Sousa <lincoln@clarete.li>
;;
;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-bullets)
(require 'org-gcal)
(require 'ob-ditaa)
(require 'ob-plantuml)


(defun lc/org/directory-dirs (dir)
  "List directories recursively inside of DIR."
  (let ((dirs (delq nil
                    (mapcar (lambda (p) (and
                                    (file-directory-p p)
                                    (not (string-prefix-p "_" (file-name-nondirectory p)))
                                    p))
                            (directory-files
                             dir t directory-files-no-dot-files-regexp)))))
    (push (expand-file-name dir) dirs)))


(defun lc/org/utf-8-bullet ()
  "Replace asterisk chars with sexy UTF-8 Bullets."
  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 ()
           (compose-region (match-beginning 1) (match-end 1) "•")))))))


(defun lc/org/keys ()
  "Key bindings for the `org-mode'."
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda))


(defun lc/org/bullets ()
  "Enable and configure `org-bullets' with custom icons."
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("▶" "▸" "▹" "▹" "▹" "▹")))


(defun lc/org/workflow ()
  "Set a kanban-ish workflow for managing TODO items."
  (setq org-todo-keywords
    '((sequence "TODO" "DOING" "BLOCKED" "|" "DONE" "ARCHIVED")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("DOING" . "yellow")
          ("BLOCKED" . org-warning)
          ("DONE" . "green")
          ("ARCHIVED" .  "blue"))))


;; TODO: Migrate to password-store
;;
;; (defun lc/org/setup-gcal ()
;;   "Setup Google Calendar integration."
;;   (setq org-gcal-client-id (car (custom-auth-url-read-user "google-oauth"))
;;         org-gcal-client-secret (car (custom-auth-url-read-password "google-oauth"))
;;         org-gcal-file-alist
;;         '(("lincoln@clarete.li" . "~/org/Calendar/lincoln@clarete.li.org"))))


(defun lc/org/babel ()
  "Setup babel `org-mode' extension."
  (setq org-ditaa-jar-path "~/.emacs.d/contrib/ditaa/ditaa0_9.jar")
  (setq org-plantuml-jar-path "~/.emacs.d/contrib/plantuml/plantuml.jar")
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (dot . t)
     (gnuplot . t)
     (latex . t)
     (plantuml . t)
     (python . t)
     ;(R . t)
     (ruby . t))))


(defun lc/org ()
  "Configuration for the `org-mode'."
  (lc/org/keys)
  (lc/org/babel)
  (lc/org/utf-8-bullet)
  (lc/org/bullets)
  (lc/org/workflow)
  (setq org-agenda-files (lc/org/directory-dirs "~/org"))
  (setq org-log-done t)
  (setq org-agenda-sticky t))


(provide 'lc-org)
;;; lc-org.el ends here
