(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Custom functions
(add-to-list 'load-path "~/.emacs.d/functions/")

(setq-default indent-tabs-mode nil
              line-spacing 0.4
              tab-width 4)

(set-face-attribute 'default nil :height 140)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode t)
(global-hl-line-mode t)
(global-linum-mode t)
(ido-mode t)

;; No backup files.
(setq make-backup-files nil)

;; (setq exec-path (append exec-path '("/usr/local/bin")))

;; Fill column.
(setq-default fill-column 80)

;; HTML offset
(defvaralias 'sgml-basic-offset 'tab-width)

;; Disable electric indent mode because it seems a bit whacked out with JSX
;; files. Probably others too!
(electric-indent-mode -1)

;; Do newline AND indent when return is pressed.
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Tabs
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Preserve case when expanding words (M-/)
(setq dabbrev-case-replace nil)

;; TODO Email
;; TODO Eww settings (search w/ DDG, defaut for browse url, etc.)
;; TODO Java settings (gtags?)
(add-hook 'java-mode-hook
  '(lambda ()
     (semantic-add-system-include (getenv "JAVA_HOME") 'java-mode)))
(semantic-mode 1)
;; company mode vs helm? M-. for ctags?

;; C mode
(setq c-default-style "linux")
(defvaralias 'c-basic-offset 'tab-width)

(use-package spacegray-theme
             :ensure t
             :config (load-theme 'spacegray t))

(use-package exec-path-from-shell
             :ensure t
             :config (when (memq window-system '(mac ns))
                       (exec-path-from-shell-initialize)))

(use-package magit
             :ensure t
             :bind (("C-x g" . magit-status)
                    ("C-x M-G" . magit-dispatch-popup))
             :config (setq magit-push-always-verify nil))

(use-package smartparens
             :ensure t
             :config
             (smartparens-global-mode t)
             (sp-local-pair 'web-mode "<" ">"))

(use-package rainbow-delimiters
             :ensure t
             :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package projectile
             :ensure t)

(use-package flycheck
             :ensure t
             :config (add-hook 'after-init-hook #'global-flycheck-mode)
                     (setq-default flycheck-disabled-checkers
                                   (append flycheck-disabled-checkers
                                           '(javascript-gjslint
                                             javascript-jshint
                                             javascript-standard
                                             json-jsonlist))))

(use-package company
             :ensure t)

(use-package ggtags
             :ensure t)

(use-package helm
             :ensure t
             :bind (("M-x" . helm-M-x)
                    ("C-x b" . helm-mini)
                    ("C-x C-f" . helm-find-files)
                    ("M-/" . helm-dabbrev)
                    ("M-y" . helm-show-kill-ring)
                    ("C-c h f" . helm-semantic-or-imenu))
             :config ;;(global-set-key (kbd "C-c h") 'helm-command-prefix)
                     ;;(global-unset-key (kbd "C-x c"))
                     (setq helm-split-window-in-side-p           t  ; open helm buffer inside current window, not occupy whole other window
                           helm-move-to-line-cycle-in-source     t  ; move to end or beginning of source when reaching top or bottom of source.
                           helm-scroll-amount                    8) ; scroll 8 lines other window using M-<next>/M-<prior>
                     (helm-mode 1))

(use-package helm-gtags
             :ensure t)

(use-package helm-company
             :ensure t)

(use-package clojure-mode
             :ensure t)

(use-package cider
             :ensure t)

(use-package web-mode
             :ensure t
             :mode ("\\.jsx$" . web-mode)
             :config (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package emmet-mode
             :ensure t)

(use-package handlebars-mode
             :ensure t)

(use-package scss-mode
             :ensure t)

(use-package js2-mode
             :ensure t
             :mode ("\\.js$" . js2-mode)
             :config
             (js2-mode-toggle-warnings-and-errors)
             (add-hook 'js2-mode-hook
                       (lambda ()
                         (let ((local-eslint (expand-file-name "node_modules/.bin/eslint" (projectile-project-root))))
                           (when (file-exists-p local-eslint)
                             (setq flycheck-javascript-eslint-executable local-eslint))))))

(use-package json-mode
             :ensure t)

(use-package json-reformat
             :ensure t)

(use-package markdown-mode
             :ensure t
             :config
             (use-package polymode
                          :ensure t
                          :config (add-hook 'poly-markdown-mode-hook 'turn-on-visual-line-mode)
                          :mode (("\\.markdown" . poly-markdown-mode)
                                 ("\\.md" . poly-markdown-mode))))

(use-package multiple-cursors
             :ensure t
             :bind (("C->" . mc/mark-next-like-this)
                    ("C-<" . mc/mark-previous-like-this)
                    ("C-c C-<" . mc/mark-all-like-this)))

(use-package haskell-mode
             :ensure t)
