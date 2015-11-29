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

(setq column-number-mode t
      global-hl-line-mode t
      global-linum-mode t
      ido-mode t
      indent-tabs-mode nil
      line-spacing 0.4
      menu-bar-mode nil
      scroll-bar-mode nil
      tab-width 4
      tool-bar-mode nil)

(set-face-attribute 'default nil :height 140)

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

(use-package rainbow-delimeters
             :ensure t
             :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package clojure-mode
             :ensure t)

(use-package cider
             :ensure t)

(use-package web-mode
             :ensure t
             :mode ("\\.jsx$" . web-mode))

(use-package emmet-mode
             :ensure t)

(use-package scss-mode
             :ensure t)

(use-package js2-mode
             :ensure t
             :mode ("\\.js$" . js2-mode))

(use-package json-mode
             :ensure t)

(use-package json-reformat
             :ensure t)

(use-package markdown-mode
             :ensure t
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

(use-package flycheck
             :ensure t
             :config
             (add-hook 'after-init-hook #'global-flycheck-mode)
             (setq-default flycheck-disabled-checkers
                           (append flycheck-disabled-checkers
                                   '(javascript-jshint json-jsonlist)))
             (flycheck-add-mode 'javascript-eslint 'web-mode))
