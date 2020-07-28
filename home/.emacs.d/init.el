;;; init --- emacs config file

;;; Commentary:

;; Emacs config file.

;;; Code:

;;; Language

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;;; Load packages

;; add the directories under site-lisp to load path
(let ((default-directory (expand-file-name "site-lisp/" user-emacs-directory)))
  (if (file-exists-p default-directory)
      (progn
        (normal-top-level-add-to-load-path '("."))
        (normal-top-level-add-subdirs-to-load-path))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; Global keybinds

(global-set-key (kbd "C-x ?") 'help-command) ; to use C-h for DEL

;;; Window balancing

(dolist (f '(split-window-below split-window-right delete-window))
  (advice-add f :after (lambda (&optional _) (balance-windows))))

;;; Customization

(customize-set-variable 'custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Other settings

(fset 'yes-or-no-p 'y-or-n-p)

(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'indicate-buffer-boundaries 'right)
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'ring-bell-function (lambda () (princ "[RING] ")))
(customize-set-variable 'scroll-conservatively 1000)
(customize-set-variable 'scroll-margin 5)
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'use-dialog-box nil)

;;; use-package

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)

(use-package ace-isearch
  :ensure t
  :after avy
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
  :diminish
  :custom (global-anzu-mode t))

(use-package apib-mode
  :ensure t
  :mode "\\.apib\\'")

(use-package autorevert
  :diminish auto-revert-mode)

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
  (company-dabbrev-downcase nil)
  :hook (after-init . global-company-mode))

(use-package csv-mode
  :ensure t)

(use-package delsel
  :config (delete-selection-mode t))

(use-package dhall-mode
  :ensure t)

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

(use-package dired-x
  :bind
  ("C-x j" . dired-jump))

(use-package dmacro
  :ensure t
  :diminish
  :custom
  (dmacro-key (kbd "C-c d"))
  :config
  (global-dmacro-mode))

(use-package dockerfile-mode
  :ensure t)

(use-package eglot
  :ensure t
  :hook
  (haskell-mode . eglot-ensure)
  (ruby-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp"))))

(use-package elec-pair
  :config
  (electric-pair-mode t))

(use-package elm-mode
  :ensure t)

(use-package emacs-lock
  :diminish
  :config
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill)))

(use-package emmet-mode
  :ensure t
  :hook (sgml-mode css-mode)
  :custom (emmet-move-cursor-between-quotes t))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package executable
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package files
  :config
  (add-to-list 'auto-mode-alist '("\\.envrc\\'" . shell-script-mode))
  (add-to-list 'backup-directory-alist '("\\.*$" . "~/.emacs.d/backup"))
  :custom
  (backup-by-copying t)
  (confirm-kill-emacs 'y-or-n-p)
  (require-final-newline 'visit))

(use-package fish-mode
  :ensure t)

(use-package frame
  :if window-system
  :init
  ;; when make-frame
  (add-hook 'after-make-frame-functions (lambda (frame)
                                          (select-window (frame-root-window frame))
                                          (split-window-horizontally)))
  :hook
  ;; when startup
  (window-setup . split-window-horizontally)
  :custom
  (default-frame-alist '((fullscreen . fullboth))))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (bind-key "C-c !" 'flycheck-list-errors flycheck-mode-map))

(use-package git-gutter
  :ensure t
  :diminish
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
  :diminish helm-migemo-mode
  :after migemo
  :config
  (helm-migemo-mode t)
  :bind
  (("C-c h" . helm-mini)
   ("C-c r" . helm-resume)
   ("M-x"   . helm-M-x)
   ("M-y"   . helm-show-kill-ring)
   ("C-x b" . helm-buffers-list)))

(use-package helm-projectile
  :ensure t
  :bind
  ("C-c j" . helm-projectile)
  ("C-c k" . helm-projectile-grep)
  :custom
  (helm-grep-file-path-style 'relative)
  (helm-projectile-sources-list
   '(helm-source-projectile-projects
     helm-source-projectile-recentf-list
     helm-source-projectile-buffers-list
     helm-source-projectile-files-list)))

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
  :custom
  (migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil))

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

(use-package peep-dired
  :ensure t
  :bind
  (:map dired-mode-map
   ("C-x x" . peep-dired)
   :map peep-dired-mode-map
   ("C-x x" . peep-dired)))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  :custom
  (projectile-use-git-grep t)
  (projectile-mode-line-prefix " P"))

(use-package purescript-mode
  :ensure t
  :init
  (add-hook 'purescript-mode-hook (lambda () (turn-on-purescript-indentation))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :hook (css-mode scss-mode html-mode lisp-mode web-mode))

(use-package recentf
  :config
  (recentf-mode t)
  :custom
  (recentf-max-saved-items 100)
  (recentf-save-file "~/.emacs.d/recentf"))

(use-package ruby-mode
  :mode
  (("\\.cap\\'" . ruby-mode)
   ("\\.Brewfile\\'" . ruby-mode))
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

(use-package scala-mode
  :ensure t)

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil))

(use-package scss-mode
  :ensure t
  :custom
  (css-indent-offset 2)
  (scss-compile-at-save nil))

(use-package selected
  :ensure t
  :config
  (bind-keys :map selected-keymap
             ("%" . query-replace)
             (";" . comment-dwim)
             ("m" . apply-macro-to-region-lines)
             ("q" . selected-off)
             ("s" . sort-lines)
             ("w" . count-words-region))
  (selected-global-mode t))

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
     ("*Help*" :align t :ratio 0.3 :select t)
     ("*xref*" :align t :size 0.3)
     ("*Flycheck errors*" :align t :size 0.3 :select t)))
  :config (shackle-mode))

(use-package shrink-whitespace
  :ensure t
  :bind
  ("M-SPC" . shrink-whitespace))

(use-package simple
  :hook
  (before-save . delete-trailing-whitespace)
  :config
  (column-number-mode t)
  :bind
  ("C-h" . delete-backward-char)
  :custom
  (kill-whole-line t))

(use-package smart-jump
  :ensure t
  :config
  (smart-jump-setup-default-registers)
  ; plain `ruby-mode' is not registered (but `robe-mode' is)
  (smart-jump-register :modes 'ruby-mode)
  ; prevent `xref-find-definitions' from falling back to etags and prompting by `visit-tags-table'
  (remove-hook 'xref-backend-functions 'etags--xref-backend))

(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t))

(use-package string-inflection
  :ensure t
  :bind ("C-c C-u" . string-inflection-all-cycle)
  :hook
  ((ruby-mode . (lambda () (local-set-key (kbd "C-c C-u") 'string-inflection-ruby-style-cycle)))
   (java-mode . (lambda () (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle)))))

(use-package terraform-mode
  :ensure t)

(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

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
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-shell "~/.nix-profile/bin/bash --login"))

(use-package vue-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2))

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode))

(use-package whitespace
  :config
  (dolist (style '(newline-mark lines tabs empty)) (delete style whitespace-style))
  (setq whitespace-display-mappings
        (seq-remove (lambda (x) (equal (seq-take x 2) '(space-mark ?\ ))) whitespace-display-mappings))
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
