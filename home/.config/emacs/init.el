;;; init --- emacs config file

;;; Commentary:

;; Emacs config file.

;;; Code:

;;; Language

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;;; Packages

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package use-package
  :custom
  (use-package-hook-name-suffix nil))

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

(use-package apib-mode
  :straight t
  :mode "\\.apib\\'")

(use-package avy
  :straight t
  :config
  (avy-setup-default))

(use-package beacon
  :straight t
  :after dim
  :config
  (dim-minor-name 'beacon-mode "")
  (beacon-mode))

(use-package browse-at-remote
  :straight t)

(use-package cc-mode
  :config
  (defun my/indent-by-two ()
    (setq-local c-basic-offset 2)
    (c-set-offset 'case-label '+))
  :hook
  (java-mode-hook . my/indent-by-two))

(use-package coffee-mode
  :straight t
  :custom
  (coffee-tab-width 2))

(use-package company
  :straight t
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
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)
  (company-dabbrev-downcase nil)
  :hook
  (after-init-hook . global-company-mode))

(use-package consult
  :straight t
  :after projectile
  :bind
  (("C-c C-r" . consult-recent-file)
   ([remap goto-line] . consult-goto-line)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop)
   :map flymake-mode-map
   ("C-c !" . consult-flymake))
  :custom
  (consult-project-root-function #'projectile-project-root)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref))

(use-package ctrlf
  :straight t
  :config
  (ctrlf-mode)
  :custom
  (ctrlf-auto-recenter t))

(use-package cursor-sensor
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode)
  :custom
  (minibuffer-prompt-properties (append minibuffer-prompt-properties '(cursor-intangible t))))

(use-package cus-edit
  :init
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (defun my/delete-custom-file ()
    (if (file-exists-p custom-file) (delete-file custom-file)))
  :hook
  (kill-emacs-hook . my/delete-custom-file)
  :custom
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil)
  (indicate-buffer-boundaries 'right)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (ring-bell-function (lambda () (princ "[RING] ")))
  (scroll-conservatively 1000)
  (scroll-margin 5)
  (set-mark-command-repeat-pop t)
  (tool-bar-mode nil)
  (use-dialog-box nil))

(use-package csv-mode
  :straight t)

(use-package delsel
  :config
  (delete-selection-mode))

(use-package dhall-mode
  :straight t)

(use-package diff-hl
  :straight t
  :demand
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode))

(use-package dim
  :straight t
  :config
  (dim-minor-name 'auto-revert-mode "" 'autorevert)
  (dim-minor-name 'ruby-end-mode "" 'ruby-end)) ; doesn't work in `use-package ruby-end`

(use-package dimmer
  :straight t
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

(use-package direnv
  :straight t
  :config
  (direnv-mode))

(use-package dmacro
  :straight t
  :after dim
  :custom
  (dmacro-key (kbd "C-c d"))
  :config
  (dim-minor-name 'dmacro-mode "")
  (global-dmacro-mode))

(use-package docker-tramp
  :straight t)

(use-package dockerfile-mode
  :straight t)

(use-package dumb-jump
  :straight t
  :hook
  (xref-backend-functions . dumb-jump-xref-activate))

(use-package duplicate-thing
  :straight t
  :bind
  ("C-c C-c" . duplicate-thing))

(use-package eglot
  :straight t
  :config
  (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
  :hook
  ((haskell-mode-hook js-mode-hook nix-mode-hook ruby-mode-hook scala-mode-hook sh-mode-hook yaml-mode-hook) . eglot-ensure)
  :bind
  (:map eglot-mode-map
   ("C-c e" . 'eglot-code-actions)))

(use-package eldoc
  :after dim
  :config
  (dim-minor-name 'eldoc-mode "" 'eldoc))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package elm-mode
  :straight t)

(use-package emacs
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package emacs-lock
  :after dim
  :config
  (dim-minor-name 'emacs-lock-mode "")
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill)))

(use-package emmet-mode
  :straight t
  :hook
  (sgml-mode-hook css-mode-hook)
  :custom
  (emmet-move-cursor-between-quotes t))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package executable
  :hook
  (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package expand-region
  :straight t
  :bind
  ("C-=" . er/expand-region))

(use-package files
  :config
  (add-to-list 'auto-mode-alist '("\\.envrc\\'" . shell-script-mode))
  (add-to-list 'backup-directory-alist `("\\.*$" . ,(expand-file-name "backup" user-emacs-directory)))
  (defun my/reload-init-file ()
    "Reload user’s initialization file."
    (interactive)
    (load-file user-init-file))
  :custom
  (backup-by-copying t)
  (confirm-kill-emacs 'y-or-n-p)
  (remote-file-name-inhibit-cache 600)
  (require-final-newline 'visit))

(use-package fish-mode
  :straight t)

(use-package flymake
  :hook
  (prog-mode-hook . flymake-mode))

(use-package flymake-diagnostic-at-point
  :straight t
  :after flymake
  :hook
  (flymake-mode-hook . flymake-diagnostic-at-point-mode)
  :custom
  (flymake-diagnostic-at-point-timer-delay 1))

(use-package frame
  :hook
  (;; when startup
   (window-setup-hook . split-window-horizontally)
   ;; when make-frame
   (after-make-frame-functions . my/split-frame-into-two-windows-horizontally))
  :config
  (blink-cursor-mode)
  (defun my/split-frame-into-two-windows-horizontally (frame)
    (select-window (frame-root-window frame))
    (split-window-horizontally))
  :custom
  (blink-cursor-blinks 0)
  (default-frame-alist '((fullscreen . fullboth) (font . "Cica-14"))))

(use-package free-keys
  :straight t)

(use-package google-this
  :straight t
  :after dim
  :config
  (dim-minor-name 'google-this-mode "")
  (google-this-mode))

(use-package goto-addr
  :hook
  ((prog-mode-hook . goto-address-prog-mode)
   (text-mode-hook . goto-address-mode)))

(use-package groovy-mode
  :straight t)

(use-package haml-mode
  :straight t
  :mode "\\.hamlc\\'")

(use-package haskell-mode
  :straight t)

(use-package help
  :bind
  ;; to use C-h for DEL
  ("C-x ?" . help-command))

(use-package highlight-indent-guides
  :straight t
  :after dim
  :hook
  (prog-mode-hook . highlight-indent-guides-mode)
  (yaml-mode-hook . highlight-indent-guides-mode)
  :config
  (dim-minor-name 'highlight-indent-guides-mode "")
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line)
  (highlight-indent-guides-responsive 'top))

(use-package hledger-mode
  :straight t
  :mode "\\.journal\\'"
  :config
  (defun my/setup-hledger-company ()
    (setq-local company-backends (cons 'hledger-company company-backends)))
  (defun my/hledger-set-tab-width ()
    (setq tab-width 4))
  :hook
  (hledger-mode-hook . my/setup-hledger-company)
  (hledger-mode-hook . my/hledger-set-tab-width))

(use-package howm
  :straight t
  :init
  (setq howm-view-title-header "#") ; 先に定義する必要がある
  :bind
  ("C-c c" . howm-menu)
  :custom
  (howm-directory "~/iCloud Drive/notes")
  (howm-file-name-format "%Y%m%d-%H%M%S.md")
  (howm-keyword-file (concat (file-name-as-directory howm-directory) ".howm-keys"))
  (howm-history-file (concat (file-name-as-directory howm-directory) ".howm-history"))
  (howm-view-split-horizontally t))

(use-package imenu-list
  :straight t
  :bind
  ("C-'" . imenu-list-smart-toggle)
  :custom
  (imenu-list-auto-resize t))

(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :custom
  (js-indent-level 2)
  (js2-indent-switch-body t)
  (js2-strict-missing-semi-warning nil))

(use-package magit
  :straight t
  :after shackle
  :demand
  :bind
  ("C-c g" . magit-status-here)
  :custom
  (magit-diff-refine-hunk 'all)
  (shackle-rules (cons '(magit-status-mode :align t :size 0.6) shackle-rules)))

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

(use-package marginalia
  :straight t
  :demand
  :config
  (marginalia-mode)
  :bind
  (:map minibuffer-local-map
   ("M-A" . marginalia-cycle)))

(use-package mb-depth
  :config
  (minibuffer-depth-indicate-mode))

(use-package migemo
  :straight t
  :config
  (migemo-init)
  :custom
  (migemo-dictionary (expand-file-name "~/.nix-profile/share/migemo/utf-8/migemo-dict"))
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-use-default-isearch-keybinding nil))

(use-package mini-modeline
  :straight t
  :after dim
  :config
  (dim-minor-name 'mini-modeline-mode "")
  (mini-modeline-mode)
  :custom
  (mini-modeline-face-attr nil)
  (mini-modeline-l-format (default-value 'mode-line-format))
  (mini-modeline-r-format nil))

(use-package multiple-cursors
  :straight t
  :bind
  (("C-c m e" . mc/edit-lines)
   ("C-c m n" . mc/mark-next-like-this)))

(use-package mwim
  :straight t
  :bind
  (([remap move-beginning-of-line] . mwim-beginning-of-line-or-code)
   ([rempa move-end-of-line] . mwim-end-of-line-or-code)))

(use-package nadvice
  :config
  (dolist (f '(split-window-below split-window-right delete-window))
  (advice-add f :after (lambda (&optional _) (balance-windows)))))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package open-junk-file
  :straight t
  :commands open-junk-file
  :custom
  (open-junk-file-format (expand-file-name "junk/%Y/%m/%d-%H%M%S." user-emacs-directory))
  (open-junk-file-find-file-function 'find-file))

(use-package org
  :commands orgtbl-mode)

(use-package peep-dired
  :straight t
  :bind
  (:map dired-mode-map
   ("C-x x" . peep-dired)
   :map peep-dired-mode-map
   ("C-x x" . peep-dired)))

(use-package projectile
  :straight t
  :demand
  :bind-keymap
  ("C-;" . projectile-command-map)
  :config
  (projectile-mode)
  :custom
  (projectile-use-git-grep t)
  (projectile-mode-line-prefix " P"))

(use-package purescript-mode
  :straight t
  :hook
  (purescript-mode-hook . turn-on-purescript-indentation))

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package rainbow-mode
  :straight t
  :hook
  (css-mode-hook scss-mode-hook html-mode-hook lisp-mode-hook web-mode-hook))

(use-package recentf
  :config
  (recentf-mode)
  :custom
  (recentf-max-saved-items 100)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory)))

(use-package restart-emacs
  :straight t
  :demand
  :bind
  ("C-x M-c" . restart-emacs)
  :config
  (defun my/restart-emacs-with-restoring-frames ()
    (interactive)
    (let ((restart-emacs-restore-frames t))
      (restart-emacs))))

(use-package ruby-mode
  :mode
  (("\\.cap\\'" . ruby-mode)
   ("\\.Brewfile\\'" . ruby-mode))
  :custom
  (ruby-insert-encoding-magic-comment nil))

(use-package ruby-end
  :straight t
  :requires ruby-mode)

(use-package ruby-hash-syntax
  :straight t)

(use-package ruby-interpolation
  :straight t
  :requires ruby-mode
  :config
  (ruby-interpolation-mode))

(use-package rust-mode
  :straight t)

(use-package savehist
  :config
  (savehist-mode))

(use-package scala-mode
  :straight t)

(use-package scroll-bar
  :custom
  (scroll-bar-mode nil))

(use-package scss-mode
  :straight t
  :custom
  (css-indent-offset 2)
  (scss-compile-at-save nil))

(use-package selected
  :straight t
  :demand
  :after dim
  :config
  (dim-minor-name 'selected-minor-mode "")
  (selected-global-mode)
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
  :straight t
  :custom
  (shackle-rules
   '(("*Warnings*" :size 0.3)
     (Buffer-menu-mode :align t :size 0.2 :select t)
     (grep-mode :align t :size 0.3 :select t)
     (help-mode :align right :size 72 :select t)
     (xref--xref-buffer-mode :align t :size 0.3)))
  :config
  (shackle-mode))

(use-package shrink-whitespace
  :straight t
  :bind
  ([remap just-one-space] . shrink-whitespace))

(use-package simple
  :hook
  (before-save-hook . delete-trailing-whitespace)
  :config
  (column-number-mode)
  :bind
  ("C-h" . delete-backward-char)
  :custom
  (kill-whole-line t))

(use-package solarized-theme
  :straight t
  :config
  (load-theme 'solarized-dark t))

(use-package subword
  :hook
  ((ruby-mode-hook . subword-mode)
   (haskell-mode-hook . subword-mode)))

(use-package string-inflection
  :straight t
  :config
  (defun my/string-inflection-for-ruby ()
    (local-set-key (kbd "C-c C-u") 'string-inflection-ruby-style-cycle))
  (defun my/string-inflection-for-java ()
    (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle))
  :bind
  ("C-c C-u" . string-inflection-all-cycle)
  :hook
  ((ruby-mode-hook . my/string-inflection-for-ruby)
   (java-mode-hook . my/string-inflection-for-java)))

(use-package terraform-mode
  :straight t)

(use-package textile-mode
  :straight t
  :mode "\\.textile\\'")

(use-package time
  :init
  (defun my/display-init-time ()
    (message "init time: %s" (emacs-init-time)))
  :hook
  (after-init-hook . my/display-init-time))

(use-package undo-tree
  :straight t
  :after (dim shackle)
  :config
  (dim-minor-name 'undo-tree-mode "")
  (global-undo-tree-mode)
  :custom
  (shackle-rules (cons '(" *undo-tree*" :align right :size 0.1 :inhibit-window-quit t) shackle-rules)))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse))

(use-package vertico
  :straight t
  :config
  (setq completion-ignore-case t) ; not a customization variable
  (vertico-mode)
  :custom
  (completion-styles (append completion-styles '(substring flex))))

(use-package vterm
  :straight t
  :custom
  (vterm-buffer-name-string "vterm: %s")
  (vterm-kill-buffer-on-exit t)
  (vterm-module-cmake-args "-DCMAKE_PREFIX_PATH=~/.nix-profile"))

(use-package vterm-toggle
  :straight t
  :after shackle
  :bind
  ("C-c v" . vterm-toggle)
  :custom
  (shackle-rules (cons '(vterm-mode :align t :size 0.5) shackle-rules))
  (vterm-toggle-scope 'project))

(use-package vue-mode
  :straight t)

(use-package web-mode
  :straight t
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2))

(use-package which-key
  :straight t
  :after dim
  :config
  (dim-minor-name 'which-key-mode "")
  (which-key-mode))

(use-package whitespace
  :after dim
  :config
  (dim-minor-name 'global-whitespace-mode "")
  (dolist (style '(newline-mark lines tabs empty)) (delete style whitespace-style))
  (customize-set-variable 'whitespace-display-mappings
                          (cons '(space-mark ?\u3000 [?\u25a1])
                                (seq-remove (lambda (x) (equal (seq-take x 2) '(space-mark ?\ ))) whitespace-display-mappings)))
  (global-whitespace-mode)
  :custom
  (whitespace-global-modes '(not vterm-mode magit-log-mode magit-status-mode)))

(use-package windmove
  :config
  (windmove-default-keybindings)
  :demand
  ;; Make windmove work in org-mode:
  :hook
  ((org-shiftup-final-hook . windmove-up)
   (org-shiftleft-final-hook . windmove-left)
   (org-shiftdown-final-hook . windmove-down)
   (org-shiftright-final-hook . windmove-right)))

(use-package winner
  :config
  (winner-mode))

(use-package yaml-mode
  :straight t)

(provide 'init)
;;; init.el ends here
