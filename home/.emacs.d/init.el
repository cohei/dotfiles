;;; init --- emacs config file

;;; Commentary:
;;; aaaa

;;; Code:

(require 'server)
(unless (server-running-p)
  (server-start))

(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function (lambda () (princ "[RING] ")))

(setq confirm-kill-emacs 'y-or-n-p)

;;;; Language

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;;;; Load libraries

;;; add the directories under site-lisp to load path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (if (file-exists-p default-directory)
      (progn
        (normal-top-level-add-to-load-path '("."))
        (normal-top-level-add-subdirs-to-load-path))))

;;; add repositories
(require 'package)
(add-to-list 'package-archives (cons "melpa-stable" "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives (cons "melpa" "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;;; Global Keybinds

(global-set-key (kbd "C-x ?") 'help-command) ; to use C-h for DEL
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x j") 'dired-jump)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;;;; Buffer Settings

;;; trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(set-default 'show-trailing-whitespace t)
(dolist (hook '(Buffer-menu-mode-hook
                eshell-mode-hook
                intero-repl-mode-hook
                package-menu-mode
                term-mode-hook
                undo-tree-visualizer-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

;;; balance-windows after spliting/deleting windows
(defun balance-windows-advice (&rest args)
  "Advice which execute `balance-window' after something.  ARGS are ignored."
  (balance-windows))
(dolist (f '(split-window-below split-window-right delete-window))
  (advice-add f :after 'balance-windows-advice))

;;; make file with shebang executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; enable goto-address-mode
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

;;; backup
(setq backup-by-copying t)
(add-to-list 'backup-directory-alist '("\\.*$" . "~/.emacs.d/backup"))

(column-number-mode)

;;;; use-package

(package-install 'use-package)
(require 'use-package)

(use-package ace-isearch
  :ensure t
  :config (global-ace-isearch-mode +1))

(use-package ace-jump-mode
  :ensure t)

(use-package align
  :bind
  ("C-c a" . align)
  ("C-x a r" . align-regexp)
  :config
  (add-to-list 'align-rules-list '(ruby19-hash (regexp . ":\\(\s-*\\)") (modes . '(ruby-mode))))
  (add-to-list 'align-rules-list '(ruby-assignment (regexp . "\\(\s-*\\)=") (modes . '(ruby-mode)))))

(use-package ansi-color
  :config
  (defun ansi-colorize-current-buffer ()
    "Colorize ansi escape sequences in the current buffer."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1))

(use-package apib-mode
  :ensure t
  :mode "\\.apib\\'")

(use-package auto-complete-config
  :ensure auto-complete
  :config
  (ac-config-default)
  (global-auto-complete-mode t)
  (setq ac-ignore-case nil))

(use-package cacoo
  :ensure t
  :bind ("M--" . toggle-cacoo-minor-mode))
;; optional
;; (use-package cacoo-plugins
;;   :config (setq cacoo:api-key "APIKEY"))

(use-package cc-mode
  :init
  (add-hook
   'java-mode-hook
   (lambda ()
     (setq c-basic-offset 2)
     (c-set-offset 'case-label '+))))

(use-package coffee-mode
  :ensure t
  :config
  (setq tab-width 2)
  (setq coffee-tab-width 2)
  (auto-complete-mode))

(use-package csv-mode
  :ensure t)

(use-package diminish
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package elm-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :defer t
  :hook (sgml-mode css-mode)
  :config
  (setq emmet-move-cursor-between-quotes t))

(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package git-gutter
  :ensure t
  :defer t
  :diminish "GG"
  :hook (prog-mode . git-gutter-mode))

(use-package haml-mode
  :ensure t
  :mode "\\.hamlc\\'")

(use-package haskell-mode
  :ensure t)

(use-package helm
  :ensure t
  :bind
  (("C-c h" . helm-mini)
   ("C-c r" . helm-resume)
   ("M-x"   . helm-M-x)
   ("M-y"   . helm-show-kill-ring)
   ("C-x b" . helm-buffers-list)))

(use-package helm-flycheck
  :ensure t
  :config
  (define-key flycheck-mode-map (kbd "C-c !") 'helm-flycheck))

(use-package helm-git-grep
  :ensure t
  :bind ("C-c k" . helm-git-grep-at-point))

(use-package helm-projectile
  :ensure t
  :bind ("C-c j" . helm-projectile))

(use-package helm-swoop
  :ensure t
  :config
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-split-direction 'split-window-vertically))

;; (helm-mode 1)
;; helm-c-source-files-in-current-dir
;; helm-mini は buffer と recentf と not-found
;; helm-imenu
;; (let ((key-and-func
;;        `((,(kbd "C-r")   helm-for-files)
;;          (,(kbd "C-^")   helm-c-apropos)
;;          (,(kbd "C-;")   helm-resume)
;;          (,(kbd "M-s")   helm-occur)
;;          (,(kbd "M-x")   helm-M-x)
;;          (,(kbd "M-y")   helm-show-kill-ring)
;;          (,(kbd "M-z")   helm-do-grep)
;;          (,(kbd "C-S-h") helm-descbinds)
;;         )))
;;   (loop for (key func) in key-and-func
;;         do (global-set-key key func)))

(use-package howm
  :ensure t
  :init
  (setq howm-view-title-header "#") ; 先に定義する必要がある
  :config
  (setq howm-directory "~/Dropbox/notes"
        howm-file-name-format "%Y%m%d-%H%M%S.md"
        howm-keyword-file (concat (file-name-as-directory howm-directory) ".howm-keys")
        howm-history-file (concat (file-name-as-directory howm-directory) ".howm-history")
        howm-view-split-horizontally t)
  :bind ("C-c c" . howm-menu))

(use-package image+
  :ensure t)

(use-package intero
  :ensure t
  :hook (haskell-mode . intero-mode))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package magit
  :ensure t
  :demand
  :bind ("C-c g" . magit-status))

(use-package maxframe
  :ensure t
  :if (memq window-system '(ns x mac)) ; cocoa, carbon -> mac, terminal -> nil, X -> x
  :init
  (defun maximize-and-split (&optional frame)
    "Maximize the window and split it horizontally into two buffers.
Optionally takes FRAME for its target and works on current frame if nothing given."
    (if frame
        (select-window (frame-root-window frame)))
    (toggle-frame-fullscreen)
    (split-window-horizontally))
  :hook
   ;; when startup
  ((window-setup . maximize-and-split)
   ;; when make-frame
   (after-make-frame-functions . maximize-and-split)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package migemo
  :ensure t
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init)
  (helm-migemo-mode 1))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m e" . mc/edit-lines)
         ("C-c m n" . mc/mark-next-like-this)))

(use-package open-junk-file
  :ensure t
  :commands open-junk-file
  :config (setq open-junk-file-format "~/.emacs.d/junk/%Y/%m/%d-%H%M%S."))

(use-package org)

(use-package projectile
  :ensure t
  :defer t ; helm-projectile will load this
  :config
  (projectile-mode)
  (setq helm-projectile-sources-list
        '(helm-source-projectile-projects
          helm-source-projectile-recentf-list
          helm-source-projectile-buffers-list
          helm-source-projectile-files-list)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :init
  :hook (css-mode scss-mode html-mode lisp-mode web-mode))

(use-package ruby-mode
  :mode (("\\.jbuilder\\'"  . ruby-mode)
         ("\\.rake\\'"      . ruby-mode)
         ("\\`Gemfile\\'"   . ruby-mode)
         ("\\`Rakefile\\'"  . ruby-mode)
         ("\\`Capfile\\'"   . ruby-mode)
         ("\\.cap\\'"       . ruby-mode)
         ("\\`Gemfile\\'"   . ruby-mode)
         ("\\`Guardfile\\'" . ruby-mode))
  :config
  (use-package ruby-end
    :ensure t
    :config (ruby-end-mode))
  (use-package ruby-interpolation
    :ensure t
    :config (ruby-interpolation-mode)))

(use-package ruby-hash-syntax
  :ensure t)

(use-package scss-mode
  :ensure t
  :config (setq css-indent-offset 2))

(use-package shackle
  :ensure t
  :config
  (setq shackle-rules
        '(("*Warnings*" :size 0.3)
          ("*Buffer List*" :size 0.3)
          ("magit:" :regexp t :align t :size 0.5)
          ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.3)
          ("*GHC Info*" :size 10)
          (" *undo-tree*" :align right :size 0.1 :inhibit-window-quit t)
          ("*git-gutter:diff*" :align t :size 0.3)
          ("\\*ag search" :regexp t :size 0.3)))
  (shackle-mode))

(use-package solarized-theme
  :ensure t
  :config
  (cond
   ((eq system-type 'darwin)
    (load-theme 'solarized-dark t))
   (t
    (load-theme 'solarized-dark t))))

(use-package string-inflection
  :ensure t
  :bind (("C-c C-u" . string-inflection-all-cycle)
         :map ruby-mode-map
         ("C-c C-u" . string-inflection-ruby-style-cycle)
         :map java-mode-map
         ("C-c C-u" . string-inflection-java-style-cycle)))

(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

(use-package tup-mode
  :ensure t)

(use-package undohist
  :ensure t
  :config (undohist-initialize))

(use-package undo-tree
  :ensure t
  :diminish ""
  :config (global-undo-tree-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'reverse))

(use-package vue-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package windmove
  :config
  (windmove-default-keybindings)
  :demand
  ;; Make windmove work in org-mode:
  :hook
  ((org-shiftup-final . windmove-up)
   (org-shiftleft-final . windmove-left)
   (org-shiftdown-final . windmove-down)
   (org-shiftright-final . windmove-right)))

(use-package yaml-mode
  :ensure t)

;;; Avoid to write `package-selected-packages` in init.el
;; (load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(dired-dwim-target t)
 '(haskell-process-type (quote stack-ghci))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-indent-switch-body t)
 '(kill-whole-line t)
 '(open-junk-file-find-file-function (quote find-file))
 '(require-final-newline (quote visit))
 '(ruby-insert-encoding-magic-comment nil)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(scroll-margin 5)
 '(scss-compile-at-save nil)
 '(tool-bar-mode nil)
 '(use-dialog-box nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here