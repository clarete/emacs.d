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
   '("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(elfeed-feeds '("http://planet.emacsen.org/atom.xml"))
 '(linum-format 'dynamic)
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "inbox-unread" :query "tag:unread AND tag:inbox")))
 '(org-agenda-files
   '("/home/lincoln/org/001-masaryk.org" "/home/lincoln/org/002-engineering.org" "/home/lincoln/org/003-personal.org"))
 '(package-selected-packages
   '(org-ditaa flycheck linum company package-lint e2ansi lsp-python-ms use-package todoist elisp-format org-static-blog password-store telega mu4e-conversation auth-source-xoauth2 mutt-mode async all-the-icons ace-window ox-json lorem-ipsum prettier-js flycheck-rust vterm gruvbox-theme toml-mode dimmer org-ehtml forth-mode php-mode ox-hugo magit doom-modeline lsp-javascript-typescript lsp-clangd lsp-treemacs company-lsp lsp-typescript lsp-python typescript-mode wisi helpful lsp-ui lsp-mode dap-mode selectric-mode graphviz-dot-mode edbi unicode-fonts csharp-mode golden-ratio bison-mode ## pretty-mode org-page clojure-mode rainbow-delimiters feature-mode dumb-jump dracula-theme posframe vlf exwm-x graphql-mode handlebars-mode vala-mode virtualenvwrapper rust-mode elfeed ess all-the-icons-dired hackernews slim-mode treemacs smart-mode-line telephone-line notmuch terraform-mode yaml-mode websocket web-mode symon sr-speedbar smex smartparens screenshot sass-mode restclient pug-mode projectile prodigy popwin persistent-soft pdf-tools pacmacs ox-reveal org-jira org-gcal org-bullets oauth2 nyan-mode nginx-mode neotree muttrc-mode multiple-cursors markdown-mode lua-mode less-css-mode jedi-direx idle-highlight-mode htmlize haskell-mode gotest flymake-cursor flycheck-pyflakes flycheck-pos-tip flycheck-cask fireplace exwm expand-region exec-path-from-shell erlang eproject ensime emojify elscreen dropdown-list drag-stuff dockerfile-mode docker csv-mode coffee-mode circe calfw auto-highlight-symbol ascii ample-theme))
 '(tramp-syntax 'default nil (tramp))
 '(user-mail-address "lincoln@clarete.li")
 '(warning-suppress-log-types
   '(((package reinitialization))
     ((package reinitialization))))
 '(warning-suppress-types '(((package reinitialization)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background nil))))
 '(org-block-begin-line ((t (:background nil))))
 '(org-block-end-line ((t (:background nil))))
 '(org-document-title ((t (:inherit default :weight bold :underline nil :background "grey15"))))
 '(org-ellipsis ((t (:foreground "#a9a1e1" :underline nil :background nil))))
 '(org-level-1 ((t (:background "transparent" :foreground "#51afef" :weight normal :height 1.0))))
 '(org-level-2 ((t (:inherit org-level-1 :foreground "#a9a1e1" :height 1.0))))
 '(org-level-3 ((t (:weight normal))))
 '(web-mode-doctype-face ((t (:foreground "#fabd2f"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#83a598"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#d3869b")))))

(provide 'custom)
;;; custom.el ends here
