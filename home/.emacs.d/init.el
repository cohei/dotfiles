;;; init --- emacs config file

;;; Commentary:

;; Emacs config file.

;;; Code:

;;; Language

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;;; Load packages

;; add the directories under site-lisp to load path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (if (file-exists-p default-directory)
      (progn
        (normal-top-level-add-to-load-path '("."))
        (normal-top-level-add-subdirs-to-load-path))))

(require 'package)
(add-to-list 'package-archives (cons "melpa-stable" "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives (cons "melpa" "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;; Global keybinds

(global-set-key (kbd "C-x ?") 'help-command) ; to use C-h for DEL
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x j") 'dired-jump)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;;; Trailing whitespaces

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(set-default 'show-trailing-whitespace t)
(dolist (hook '(Buffer-menu-mode-hook
                eshell-mode-hook
                package-menu-mode
                term-mode-hook
                undo-tree-visualizer-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

;;; Window balancing

(dolist (f '(split-window-below split-window-right delete-window))
  (advice-add f :after 'balance-windows))

;;; Make executable

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; Backup

(customize-set-variable 'backup-by-copying t)
(add-to-list 'backup-directory-alist '("\\.*$" . "~/.emacs.d/backup"))

;;; direnv

(add-to-list 'auto-mode-alist '("\\.envrc\\'" . shell-script-mode))

;;; Customization

(customize-set-variable 'custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Other settings

(column-number-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(customize-set-variable 'confirm-kill-emacs 'y-or-n-p)
(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'kill-whole-line t)
(customize-set-variable 'require-final-newline 'visit)
(customize-set-variable 'ring-bell-function (lambda () (princ "[RING] ")))
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'scroll-conservatively 1000)
(customize-set-variable 'scroll-margin 5)
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'use-dialog-box nil)

;;; use-package

(package-install 'use-package)
(require 'use-package)

(use-package ace-isearch
  :ensure t
  :requires avy
  :custom
  (global-ace-isearch-mode t)
  (ace-isearch-function 'avy-goto-char))

(use-package align
  :bind
  ("C-c a" . align)
  ("C-x a r" . align-regexp)
  :config
  (add-to-list 'align-rules-list '(ruby19-hash (regexp . ":\\(\s-*\\)") (modes . '(ruby-mode))))
  (add-to-list 'align-rules-list '(ruby-assignment (regexp . "\\(\s-*\\)=") (modes . '(ruby-mode)))))

(use-package ansi-color
  :config
  (defun my/ansi-colorize-current-buffer ()
    "Colorize ansi escape sequences in the current buffer."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package anzu
  :ensure t
  :custom (global-anzu-mode t))

(use-package apib-mode
  :ensure t
  :mode "\\.apib\\'")

(use-package avy
  :ensure t)

(use-package beacon
  :ensure t
  :config (beacon-mode 1))

(use-package cc-mode
  :hook
  (java-mode . (lambda ()
                 (setq c-basic-offset 2)
                 (c-set-offset 'case-label '+))))

(use-package coffee-mode
  :ensure t
  :custom (coffee-tab-width 2))

(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0)
  :hook (after-init . global-company-mode))

(use-package csv-mode
  :ensure t)

(use-package delsel
  :config (delete-selection-mode t))

(use-package diminish
  :ensure t)

(use-package dimmer
  :ensure t
  :config (dimmer-mode)
  :custom
  (dimmer-exclusion-regexp "^\\*helm")
  (dimmer-fraction 0.3))

(use-package dired
  :custom (dired-dwim-target t))

(use-package dockerfile-mode
  :ensure t)

(use-package dumb-jump
  :ensure t
  :config (dumb-jump-mode))

(use-package eglot
  :ensure t
  :hook (ruby-mode . eglot-ensure))

(use-package elm-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :defer t
  :hook (sgml-mode css-mode)
  :custom (emmet-move-cursor-between-quotes t))

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

(use-package goto-addr
  :hook
  ((prog-mode . goto-address-prog-mode)
   (text-mode . goto-address-mode)))

(use-package haml-mode
  :ensure t
  :mode "\\.hamlc\\'")

(use-package haskell-mode
  :ensure t
  :custom (haskell-stylish-on-save t))

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
  :bind (:map flycheck-mode-map ("C-c !" . helm-flycheck)))

(use-package helm-git-grep
  :ensure t
  :bind ("C-c k" . helm-git-grep-at-point))

(use-package helm-projectile
  :ensure t
  :bind ("C-c j" . helm-projectile))

(use-package howm
  :ensure t
  :init (setq howm-view-title-header "#") ; 先に定義する必要がある
  :bind ("C-c c" . howm-menu)
  :custom
  (howm-directory "~/Dropbox/notes")
  (howm-file-name-format "%Y%m%d-%H%M%S.md")
  (howm-keyword-file (concat (file-name-as-directory howm-directory) ".howm-keys"))
  (howm-history-file (concat (file-name-as-directory howm-directory) ".howm-history"))
  (howm-view-split-horizontally t))

(use-package image+
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :custom
  (js-indent-level 2)
  (js2-indent-switch-body t)
  (js2-strict-missing-semi-warning nil))

(use-package magit
  :ensure t
  :demand
  :bind ("C-c g" . magit-status))

(use-package maxframe
  :ensure t
  :if (memq window-system '(ns x mac)) ; cocoa, carbon -> mac, terminal -> nil, X -> x
  :init
  (defun my/maximize-and-split (&optional frame)
    "Maximize the window and split it horizontally into two buffers.
Optionally takes FRAME for its target and works on current frame if nothing given."
    (if frame
        (select-window (frame-root-window frame)))
    (toggle-frame-fullscreen)
    (split-window-horizontally))
  :hook
   ;; when startup
  ((window-setup . my/maximize-and-split)
   ;; when make-frame
   (after-make-frame-functions . my/maximize-and-split)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package migemo
  :ensure t
  :config
  (migemo-init)
  (helm-migemo-mode 1)
  :custom
  (migemo-command "cmigemo")
  (migemo-options '("-q" "--emacs"))
  (migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c m e" . mc/edit-lines)
   ("C-c m n" . mc/mark-next-like-this)))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package open-junk-file
  :ensure t
  :commands open-junk-file
  :custom
  (open-junk-file-format "~/.emacs.d/junk/%Y/%m/%d-%H%M%S.")
  (open-junk-file-find-file-function 'find-file))

(use-package org)

(use-package projectile
  :ensure t
  :defer t ; helm-projectile will load this
  :config (projectile-mode)
  :custom
  (helm-projectile-sources-list
   '(helm-source-projectile-projects
     helm-source-projectile-recentf-list
     helm-source-projectile-buffers-list
     helm-source-projectile-files-list)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :hook (css-mode scss-mode html-mode lisp-mode web-mode))

(use-package ruby-mode
  :mode
  (("\\.jbuilder\\'"  . ruby-mode)
   ("\\.rake\\'"      . ruby-mode)
   ("\\`Gemfile\\'"   . ruby-mode)
   ("\\`Rakefile\\'"  . ruby-mode)
   ("\\`Capfile\\'"   . ruby-mode)
   ("\\.cap\\'"       . ruby-mode)
   ("\\`Guardfile\\'" . ruby-mode))
  :custom (ruby-insert-encoding-magic-comment nil))

(use-package ruby-end
  :ensure t
  :requires ruby-mode)

(use-package ruby-hash-syntax
  :ensure t)

(use-package ruby-interpolation
  :ensure t
  :requires ruby-mode
  :config (ruby-interpolation-mode))

(use-package rust-mode
  :ensure t)

(use-package scss-mode
  :ensure t
  :custom
  (css-indent-offset 2)
  (scss-compile-at-save nil))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package shackle
  :ensure t
  :custom
  (shackle-rules
   '(("*Warnings*" :size 0.3)
     ("*Buffer List*" :size 0.3)
     ("magit:" :regexp t :align t :size 0.5)
     ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.3)
     ("*GHC Info*" :size 10)
     (" *undo-tree*" :align right :size 0.1 :inhibit-window-quit t)
     ("*git-gutter:diff*" :align t :size 0.3)
     ("\\*ag search" :regexp t :size 0.3)
     ("*Help*" :align t :ratio 0.3 :select t)))
  :config (shackle-mode))

(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t))

(use-package string-inflection
  :ensure t
  :bind ("C-c C-u" . string-inflection-all-cycle)
  :hook
  ((ruby-mode . (lambda () (local-set-key (kbd "C-c C-u") 'string-inflection-ruby-style-cycle)))
   (java-mode . (lambda () (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle)))))

(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

(use-package tup-mode
  :ensure t)

(use-package undohist
  :ensure t
  :config (undohist-initialize)
  :custom (undohist-ignored-files "PULLREQ_EDITMSG"))

(use-package undo-tree
  :ensure t
  :diminish ""
  :config (global-undo-tree-mode))

(use-package uniquify
  :custom (uniquify-buffer-name-style 'reverse))

(use-package vterm
  :ensure t
  :bind ("C-c v" . vterm)
  :custom (vterm-shell "~/.nix-profile/bin/bash --login"))

(use-package vue-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package whitespace
  :config
  (dolist (style '(newline-mark lines tabs empty)) (delete style whitespace-style))
  (setq whitespace-display-mappings
        (seq-remove (lambda (x) (eql (car x) 'space-mark)) whitespace-display-mappings))
  (add-to-list 'whitespace-display-mappings '(space-mark ?\u3000 [?\u25a1]))
  (global-whitespace-mode 1)
  :custom (whitespace-global-modes '(not vterm-mode)))

(use-package windmove
  :config (windmove-default-keybindings)
  :demand
  ;; Make windmove work in org-mode:
  :hook
  ((org-shiftup-final . windmove-up)
   (org-shiftleft-final . windmove-left)
   (org-shiftdown-final . windmove-down)
   (org-shiftright-final . windmove-right)))

(use-package yaml-mode
  :ensure t)

(provide 'init)
;;; init.el ends here
