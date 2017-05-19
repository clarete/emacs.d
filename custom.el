;;; custom.el --- My Emacs Setup
;;
;;; Commentary:
;;
;; Emacs adds this `custom-set-variables` somewhere and I didn't want
;; it to be anywhere I need to see.
;;
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "inbox-unread" :query "tag:unread AND tag:inbox"))))
 '(package-selected-packages
   (quote
    (notmuch terraform-mode yaml-mode websocket web-mode use-package symon sr-speedbar smex smartparens smart-mode-line screenshot sass-mode restclient pug-mode projectile prodigy popwin persistent-soft pdf-tools pallet pacmacs ox-reveal org-plus-contrib org-jira org-gcal org-bullets oauth2 nyan-mode nginx-mode neotree muttrc-mode multiple-cursors markdown-mode magit lua-mode less-css-mode jedi-direx idle-highlight-mode htmlize haskell-mode hackernews gotest flymake-cursor flycheck-pyflakes flycheck-pos-tip flycheck-cask fireplace exwm expand-region exec-path-from-shell erlang eproject ensime emojify elscreen dropdown-list drag-stuff dockerfile-mode docker csv-mode coffee-mode circe calfw auto-highlight-symbol ascii ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'custom)
;;; custom.el ends here
