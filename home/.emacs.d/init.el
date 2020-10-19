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

(defalias 'yes-or-no-p 'y-or-n-p)

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

(use-package align
  :bind
  ("C-c a" . align)
  ("C-x a r" . align-regexp)
  :config
  (add-to-list 'align-rules-list '(ruby19-hash (regexp . ":\\(\s-*\\)") (modes . '(ruby-mode))))
  (add-to-list 'align-rules-list '(ruby-assignment (regexp . "\\(\s-*\\)=") (modes . '(ruby-mode)))))

(use-package amx
  :ensure t
  :demand
  :config
  (amx-mode t)
  :bind
  ("M-X" . amx-major-mode-commands))

(use-package ansi-color
  :config
  (defun my/ansi-colorize-current-buffer ()
    "Colorize ansi escape sequences in the current buffer."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package anzu
  :ensure t
  :after dim
  :config
  (dim-minor-name 'anzu-mode "")
  (global-anzu-mode t))

(use-package apib-mode
  :ensure t
  :mode "\\.apib\\'")

(use-package avy
  :ensure t
  :config
  (avy-setup-default))

(use-package beacon
  :ensure t
  :after dim
  :config
  (dim-minor-name 'beacon-mode "")
  (beacon-mode t))

(use-package cc-mode
  :config
  (defun my/indent-by-two ()
    (setq c-basic-offset 2)
    (c-set-offset 'case-label '+))
  :hook
  (java-mode . my/indent-by-two))

(use-package coffee-mode
  :ensure t
  :custom
  (coffee-tab-width 2))

(use-package company
  :ensure t
  :after dim
  :config
  (dim-minor-name 'company-mode "")
  :bind
  (:map company-active-map
   ("<tab>" . company-complete-common-or-cycle)
   ("M-p" . nil)
   ("M-n" . nil)
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0)
  (company-dabbrev-downcase nil)
  :hook
  (after-init . global-company-mode))

(use-package csv-mode
  :ensure t)

(use-package delsel
  :config
  (delete-selection-mode t))

(use-package dhall-mode
  :ensure t)

(use-package dim
  :ensure t
  :config
  (dim-minor-name 'auto-revert-mode "" 'autorevert))

(use-package dimmer
  :ensure t
  :config
  (dimmer-mode)
  :custom
  (dimmer-fraction 0.3))

(use-package dired
  :custom
  (dired-dwim-target t))

(use-package dired-x
  :bind
  ("C-x j" . dired-jump))

(use-package dmacro
  :ensure t
  :after dim
  :custom
  (dmacro-key (kbd "C-c d"))
  :config
  (dim-minor-name 'dmacro-mode "")
  (global-dmacro-mode))

(use-package dockerfile-mode
  :ensure t)

(use-package eglot
  :ensure t
  :hook
  ((haskell-mode ruby-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  :bind
  (:map eglot-mode-map
   ("C-c e" . 'eglot-code-actions)))

(use-package elec-pair
  :config
  (electric-pair-mode t))

(use-package elm-mode
  :ensure t)

(use-package emacs-lock
  :after dim
  :config
  (dim-minor-name 'emacs-lock-mode "")
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill)))

(use-package emmet-mode
  :ensure t
  :hook
  (sgml-mode css-mode)
  :custom
  (emmet-move-cursor-between-quotes t))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package executable
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

(use-package files
  :config
  (add-to-list 'auto-mode-alist '("\\.envrc\\'" . shell-script-mode))
  (add-to-list 'backup-directory-alist '("\\.*$" . "~/.emacs.d/backup"))
  (defun my/reload-init-file ()
    "Reload user’s initialization file."
    (interactive)
    (load-file user-init-file))
  :custom
  (backup-by-copying t)
  (confirm-kill-emacs 'y-or-n-p)
  (require-final-newline 'visit))

(use-package fish-mode
  :ensure t)

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode t)
  :custom
  ;; disable ido faces to see flx highlights.
  (ido-use-faces nil))

(use-package flycheck
  :ensure t
  :after shackle
  :demand
  :config
  (global-flycheck-mode t)
  :bind
  (:map flycheck-mode-map
   ("C-c !" . flycheck-list-errors))
  :custom
  (shackle-rules (cons '("*Flycheck errors*" :align t :size 0.3 :select t) shackle-rules)))

(use-package frame
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

(use-package free-keys
  :ensure t)

(use-package git-gutter
  :ensure t
  :after (dim shackle)
  :hook
  (prog-mode . git-gutter-mode)
  :config
  (dim-minor-name 'git-gutter-mode "")
  :custom
  (shackle-rules (cons '("*git-gutter:diff*" :align t :size 0.3) shackle-rules)))

(use-package google-this
  :ensure t
  :after dim
  :config
  (dim-minor-name 'google-this-mode "")
  (google-this-mode t))

(use-package goto-addr
  :hook
  ((prog-mode . goto-address-prog-mode)
   (text-mode . goto-address-mode)))

(use-package haml-mode
  :ensure t
  :mode "\\.hamlc\\'")

(use-package haskell-mode
  :ensure t
  :custom
  (haskell-stylish-on-save t))

(use-package highlight-indent-guides
  :ensure t
  :after dim
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (dim-minor-name 'highlight-indent-guides-mode "")
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line)
  (highlight-indent-guides-responsive 'top))

(use-package hledger-mode
  :ensure t
  :mode "\\.journal\\'"
  :config
  (defun my/setup-hledger-company ()
    (setq-local company-backends (cons 'hledger-company company-backends)))
  :hook
  (hledger-mode . my/setup-hledger-company))

(use-package howm
  :ensure t
  :init
  (setq howm-view-title-header "#") ; 先に定義する必要がある
  :bind
  ("C-c c" . howm-menu)
  :custom
  (howm-directory "~/Dropbox/notes")
  (howm-file-name-format "%Y%m%d-%H%M%S.md")
  (howm-keyword-file (concat (file-name-as-directory howm-directory) ".howm-keys"))
  (howm-history-file (concat (file-name-as-directory howm-directory) ".howm-history"))
  (howm-view-split-horizontally t))

(use-package ido
  :demand
  :config
  (ido-mode t)
  (defun my/ido-recentf ()
    (interactive)
    (find-file (ido-completing-read "Find recent file: " recentf-list)))
  :bind
  ("C-c C-r" . my/ido-recentf)
  :custom
  (ido-enable-flex-matching t)
  (ido-auto-merge-work-directories-length -1))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode t))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode t)
  :custom
  (ido-vertical-show-count t)
  (ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(use-package image+
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :custom
  (js-indent-level 2)
  (js2-indent-switch-body t)
  (js2-strict-missing-semi-warning nil))

(progn
  (unless (package-installed-p 'kill-ring-ido)
    (with-current-buffer
        (url-retrieve-synchronously
         "http://www.emacswiki.org/emacs/download/kill-ring-ido.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (insert ";;; kill-ring-ido.el ends here\n")
      (package-install-from-buffer)))

  (use-package kill-ring-ido
    :init
    (use-package noflet
      :ensure t)
    :bind
    ("M-y" . kill-ring-ido)))

(use-package magit
  :ensure t
  :after shackle
  :demand
  :bind
  ("C-c g" . magit-status)
  :custom
  (magit-completing-read-function 'magit-ido-completing-read)
  (shackle-rules (cons '("magit:" :regexp t :align t :size 0.5) shackle-rules)))

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
  (migemo-dictionary (expand-file-name "~/.nix-profile/share/migemo/utf-8/migemo-dict"))
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

(use-package org
  :commands orgtbl-mode)

(use-package peep-dired
  :ensure t
  :bind
  (:map dired-mode-map
   ("C-x x" . peep-dired)
   :map peep-dired-mode-map
   ("C-x x" . peep-dired)))

(use-package projectile
  :ensure t
  :bind-keymap
  ("C-;" . projectile-command-map)
  :config
  (projectile-mode t)
  :custom
  (projectile-use-git-grep t)
  (projectile-mode-line-prefix " P"))

(use-package purescript-mode
  :ensure t
  :hook
  (purescript-mode . turn-on-purescript-indentation))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :hook
  (css-mode scss-mode html-mode lisp-mode web-mode))

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
  :custom
  (ruby-insert-encoding-magic-comment nil))

(use-package ruby-end
  :ensure t
  :requires ruby-mode)

(use-package ruby-hash-syntax
  :ensure t)

(use-package ruby-interpolation
  :ensure t
  :requires ruby-mode
  :config
  (ruby-interpolation-mode t))

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
  :demand
  :after dim
  :config
  (dim-minor-name 'selected-minor-mode "")
  (selected-global-mode t)
  :bind
  (:map selected-keymap
   ("%" . query-replace)
   (";" . comment-dwim)
   ("g" . google-this-region)
   ("m" . apply-macro-to-region-lines)
   ("q" . selected-off)
   ("s" . sort-lines)
   ("w" . count-words-region)))

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
     ("*Help*" :align t :ratio 0.3 :select t)
     ("*xref*" :align t :size 0.3)
     ("*grep*" :align t :size 0.3 :select t)))
  :config
  (shackle-mode t))

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
  :config
  (load-theme 'solarized-dark t))

(use-package string-inflection
  :ensure t
  :config
  (defun my/string-inflection-for-ruby ()
    (local-set-key (kbd "C-c C-u") 'string-inflection-ruby-style-cycle))
  (defun my/string-inflection-for-java ()
    (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle))
  :bind
  ("C-c C-u" . string-inflection-all-cycle)
  :hook
  ((ruby-mode . my/string-inflection-for-ruby)
   (java-mode . my/string-inflection-for-java)))

(use-package swoop
  :ensure t
  :custom
  (swoop-window-split-current-window: t)
  (swoop-font-size-change: nil)
  :bind
  (:map isearch-mode-map ("C-o" . swoop-from-isearch)
   :map swoop-map ("C-o" . swoop-multi-from-swoop)))

(use-package terraform-mode
  :ensure t)

(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

(use-package time
  :init
  (defun my/display-init-time ()
    (message "init time: %s" (emacs-init-time)))
  :hook
  (after-init . my/display-init-time))

(use-package undohist
  :ensure t
  :config
  (undohist-initialize)
  :custom
  (undohist-ignored-files "PULLREQ_EDITMSG"))

(use-package undo-tree
  :ensure t
  :after (dim shackle)
  :config
  (dim-minor-name 'undo-tree-mode "")
  (global-undo-tree-mode t)
  :custom
  (shackle-rules (cons '(" *undo-tree*" :align right :size 0.1 :inhibit-window-quit t) shackle-rules)))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse))

(use-package vterm
  :ensure t
  :bind
  ("C-c v" . vterm)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-module-cmake-args "-DCMAKE_PREFIX_PATH=~/.nix-profile")
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
  :after dim
  :config
  (dim-minor-name 'which-key-mode "")
  (which-key-mode t))

(use-package whitespace
  :after dim
  :config
  (dim-minor-name 'global-whitespace-mode "")
  (dolist (style '(newline-mark lines tabs empty)) (delete style whitespace-style))
  (setq whitespace-display-mappings
        (seq-remove (lambda (x) (equal (seq-take x 2) '(space-mark ?\ ))) whitespace-display-mappings))
  (add-to-list 'whitespace-display-mappings '(space-mark ?\u3000 [?\u25a1]))
  (global-whitespace-mode t)
  :custom
  (whitespace-global-modes '(not vterm-mode)))

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

(provide 'init)
;;; init.el ends here
