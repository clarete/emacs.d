;;; lc-org.el --- Setup for org-mode
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
;; Some code to help out with org-mode setup.
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
