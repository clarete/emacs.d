;;; custom-auth.el --- Password and Authentication Setup
;;
;;; Commentary:
;;
;; Here's the central place for password and authentication settings.
;; All the setup is based on the following guide:
;;
;;  https://www.gnu.org/software/emacs/manual/html_mono/auth.html
;;
;;; Code:
(require 'auth-source)

(defun custom-auth-service (service)
  "Search for SERVICE in auth-source."
  (auth-source-search
   :host (concat service ":443")
   :port "https"
   :require '(:user :secret)))

(defun custom-auth-url-read-user (service)
  "Retrieve user for SERVICE from auth-source."
  (let ((auth-list (custom-auth-service service)))
    (mapcar
     #'(lambda (p) (plist-get p :user)) auth-list)))

(defun custom-auth-url-read-password (service)
  "Retrieve password for SERVICE from auth-source."
  (let ((auth-list (custom-auth-service service)))
    (mapcar
     #'(lambda (p) (let ((secret (plist-get p :secret)))
                (if (functionp secret)
                    (funcall secret)
                  secret)))
         auth-list)))

(defun custom-auth ()
  "Execute Password and Authentication settings."
  (setq auth-sources '("~/.authinfo.gpg")))

(provide 'custom-auth)
;;; custom-auth.el ends here
