;;; custom-speedbar.el --- My speedbar setup
;;
;;; Commentary:
;;
;; Personal setup for sr-speedbar
;;
;;; Code:

(require 'sr-speedbar)

(defun custom-speedbar ()
  "Setup for the Emacs speed-bar."
  (setq sr-speedbar-right-side nil)
  (setq speedbar-show-unknown-files t)
  (setq speedbar-use-images nil)
  (setq speedbar-show-unknown-files t)
  (setq speedbar-smart-directory-expand-flag nil)
  (setq sr-speedbar-auto-refresh nil)
  (setq speedbar-frame-parameters
        '((minibuffer)
          (width . 40)
          (border-width . 0)
          (menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (unsplittable . t)
          (left-fringe . 0)))
  (add-hook 'go-mode-hook
            (lambda ()
              (speedbar-add-supported-extension ".go")
              (setq imenu-generic-expression
                    '((nil "^type *\\([^ \t\n\r\f]*\\)" 1)
                      (nil "^func *\\(.*\\) {" 1)
                      (nil "^def *\\([^\(]\\):" 1)
                      (nil "^class *\\([^\(]\\):" 1))
                    imenu-case-fold-search nil
                    speedbar-tag-hierarchy-method nil)
              (imenu-add-to-menubar "Index")))
  (global-set-key [(ctrl c) (ctrl tab)] 'sr-speedbar-toggle))


(provide 'custom-speedbar)
;;; custom-speedbar.el ends here
