(package-initialize)

;; No backup files.
(setq make-backup-files nil)

;; (setq exec-path (append exec-path '("/usr/local/bin")))

;; Use path from shell.
(exec-path-from-shell-initialize)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(setq magit-push-always-verify nil)

;; Disable electric indent mode because it seems a bit whacked out with JSX
;; files. Probably others too!
(electric-indent-mode -1)

;; Do newline AND indent when return is pressed.
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Smartparens
(smartparens-global-mode t)
(sp-local-pair 'web-mode "<" ">")

;; Rainbow delimeters.
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Fill column.
(setq-default fill-column 80)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))

;; Use polymarkdown-mode for .md and .markdown files.
;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-hook 'poly-markdown-mode-hook 'turn-on-visual-line-mode)

;; Multiple cusors.
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Tabs
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; (setq-default flycheck-eslintrc "~/.eslintrc")

;; TODO: Fix PATH for emacs...
;; (setq-default flycheck-javascript-eslint-executable "/usr/local/bin/eslint")

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; C mode
(setq c-default-style "linux")
(defvaralias 'c-basic-offset 'tab-width)

;; TODO: Figure out a better way to organise this. Custom functions in .emacs? Hrm....
(defun me/convert-ascii-to-unicode (&optional b e)
  "Converts ascii punctuation marks (quotes, dashes, and ellipses) into their unicode equivilents."
  (interactive "r")
  (let ((output-buffer (generate-new-buffer "*ASCII to Unicode Output*")))
    (call-process-region b e "/usr/local/bin/smartypants" nil output-buffer nil "-2")
    (set-buffer output-buffer)
    (call-process-region (point-min) (point-max) "/usr/local/bin/ascii2uni" t output-buffer nil "-a" "D" "-q")
    (switch-to-buffer-other-window output-buffer)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (spacegray)))
 '(custom-safe-themes
   (quote
    ("4ff23437b3166eeb7ca9fa026b2b030bba7c0dfdc1ff94df14dfb1bcaee56c78" default)))
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(line-spacing 0.4)
 '(menu-bar-mode nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140)))))
