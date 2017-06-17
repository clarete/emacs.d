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
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
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
    (spaceline-all-the-icons smart-mode-line telephone-line notmuch terraform-mode yaml-mode websocket web-mode use-package symon sr-speedbar smex smartparens screenshot sass-mode restclient pug-mode projectile prodigy popwin persistent-soft pdf-tools pallet pacmacs ox-reveal org-plus-contrib org-jira org-gcal org-bullets oauth2 nyan-mode nginx-mode neotree muttrc-mode multiple-cursors markdown-mode magit lua-mode less-css-mode jedi-direx idle-highlight-mode htmlize haskell-mode hackernews gotest flymake-cursor flycheck-pyflakes flycheck-pos-tip flycheck-cask fireplace exwm expand-region exec-path-from-shell erlang eproject ensime emojify elscreen dropdown-list drag-stuff dockerfile-mode docker csv-mode coffee-mode circe calfw auto-highlight-symbol ascii ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:background nil :foreground "#bbc2cf")) (((class color) (min-colors 256)) (:background nil :foreground "#bfbfbf")) (((class color) (min-colors 16)) (:background nil :foreground "brightwhite"))))
 '(org-block ((t (:background nil))))
 '(org-block-begin-line ((t (:background nil))))
 '(org-block-end-line ((t (:background nil))))
 '(org-ellipsis ((t (:foreground "#a9a1e1" :underline nil :background nil))))
 '(org-level-1 ((t (:background "transparent" :foreground "#51afef" :weight normal :height 1.0))))
 '(org-level-2 ((t (:inherit org-level-1 :foreground "#a9a1e1" :height 1.0))))
 '(org-level-3 ((t (:weight normal)))))

(provide 'custom)
;;; custom.el ends here
