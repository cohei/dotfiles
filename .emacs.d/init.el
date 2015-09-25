;;; init --- emacs config file

;;; Commentary:
;;; aaaa

;;; Code:

(require 'server)
(unless (server-running-p)
  (server-start))

;; language settings
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; add the directories under site-lisp to load path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; add repositories
(require 'package)
(add-to-list 'package-archives (cons "melpa-stable" "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives (cons "melpa" "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; global keybinds
(global-set-key (kbd "C-x ?") 'help-command) ; to use C-h for DEL
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x j") 'dired-jump)
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; show trailing whitespaces
(dolist (hook '(term-mode-hook undo-tree-visualizer-mode-hook Buffer-menu-mode-hook eshell-mode-hook
                package-menu-mode))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; bell
(when (eq window-system 'ns)
  (setq ring-bell-function 'ignore))

;; balance-windows after spliting/deleting windows
(defun balance-windows-advice (&rest args)
  "Advice which execute `balance-window' after something.  ARGS are ignored."
  (balance-windows))
(dolist (f '(split-window-below split-window-right delete-window))
  (advice-add f :after 'balance-windows-advice))

;; ----------------------------------------
;; use-package
(package-install 'use-package)
(require 'use-package)

(use-package align
  :config
  (global-set-key (kbd "C-c a") 'align)
  (global-set-key (kbd "C-x a r") 'align-regexp)
  (add-to-list 'align-rules-list
               '(ruby19-hash (regexp . ":\\(\s-*\\)") (modes . '(ruby-mode))))
  (add-to-list 'align-rules-list
               '(ruby-assignment (regexp . "\\(\s-*\\)=") (modes . '(ruby-mode)))))

(use-package ansi-color
  :config
  (defun ansi-colorize-current-buffer ()
    "Colorize ansi escape sequences in the current buffer."
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

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

(use-package coffee-mode
  :ensure t
  :config
  (setq tab-width 2)
  (setq coffee-tab-width 2)
  (auto-complete-mode))

(use-package emmet-mode
  :ensure t
  :defer t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t))

(use-package ensime
  :ensure t
  :defer t
  ;; This step causes the ensime-mode to be started whenever
  ;; scala-mode is started for a buffer. You may have to customize this step
  ;; if you're not using the standard scala mode.
  :config (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package ghc
  :ensure t
  :commands (ghc-init ghc-debug)
  :config (add-to-list 'ac-sources 'ac-source-ghc-mod))

(use-package git-gutter
  :ensure t
  :defer t
  :diminish "GG"
  :init (add-hook 'prog-mode-hook 'git-gutter-mode))

(use-package haml-mode
  :ensure t
  :mode "\\.hamlc\\'")

(use-package haskell-mode
  :ensure t
  :defer t
  :init
  (add-hook 'haskell-mode-hook
            '(lambda ()
               (haskell-indentation-mode 1)
               (flycheck-mode -1) ; to cancel global flycheck mode
               (ghc-init)))
  :config
  (bind-keys :map haskell-mode-map
             ("C-x C-d" . nil)
             ("C-c C-z" . haskell-interactive-switch)
             ("C-c C-l" . haskell-process-load-file)
             ("C-c C-b" . haskell-interactive-switch)
             ;; ("C-c C-t" . haskell-process-do-type)
             ;; ("C-c C-i" . haskell-process-do-info)
             ("C-c M-." . nil)
             ("C-c C-d" . nil)))

(use-package helm
  :ensure t
  :bind
  (("C-c h" . helm-mini)
   ("M-x"   . helm-M-x)
   ("M-y"   . helm-show-kill-ring)
   ("C-x b" . helm-buffers-list)))

(use-package helm-flycheck
  :ensure t
  :config
  (define-key flycheck-mode-map (kbd "C-c !") 'helm-flycheck))

(use-package helm-projectile
  :ensure t
  :bind ("C-c r" . helm-projectile))

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

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :config (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package maxframe
  :if (memq window-system '(ns x)) ; cocoa, carbon -> mac, terminal -> nil, X -> x
  :config
  (defun maximize-and-split (&optional frame)
    "Maximize the window and split it horizontally into two buffers.
Optionally takes FRAME for its target and works on current frame if nothing given."
    (if frame
        (select-window (frame-root-window frame)))
    (toggle-frame-fullscreen)
    (split-window-horizontally))
  ;; when startup
  (add-hook 'window-setup-hook          'maximize-and-split t)
  ;; when make-frame
  (add-hook 'after-make-frame-functions 'maximize-and-split t))

(use-package markdown-mode
  :ensure t
  :mode "\\.markdown\\'")

(use-package migemo
  :ensure t
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))

(use-package open-junk-file
  :ensure t
  :commands open-junk-file
  :config (setq open-junk-file-format "~/.emacs.d/junk/%Y/%m/%d-%H%M%S."))

(use-package org)

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (push '("*Warnings*" :height 0.3) popwin:special-display-config)
  (push '("*Buffer List*" :height 0.3) popwin:special-display-config)
  (push '("\\*magit" :regexp t :height 0.5) popwin:special-display-config)
  (push '("\\*helm" :regexp t) popwin:special-display-config)
  (push '("*GHC Info*" :height 10) popwin:special-display-config)
  (push '(" *undo-tree*" :width 0.1 :position right) popwin:special-display-config)
  (push '("*git-gutter:diff*" :height 0.3 :stick t) popwin:special-display-config)
  (push '("\\*ag search" :regexp t :height 0.3 :stick t) popwin:special-display-config))

(use-package projectile
  :ensure t
  :defer t ; helm-projectile will load this
  :config
  (projectile-global-mode)
  (setq helm-projectile-sources-list
        '(helm-source-projectile-projects
          helm-source-projectile-recentf-list
          helm-source-projectile-buffers-list
          helm-source-projectile-files-list)))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :init
  (dolist (hook '(css-mode-hook scss-mode-hook html-mode-hook lisp-mode-hook web-mode-hook))
    (add-hook hook (lambda () (rainbow-mode 1)))))

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

(use-package inf-ruby :commands inf-ruby)
(use-package rubydb3x :commands rubydb)

(use-package ruby-hash-syntax
  :ensure t)

(use-package scss-mode
  :ensure t
  :config (setq css-indent-offset 2))

(use-package solarized-theme
  :ensure t
  :config
  (cond
   ((eq system-type 'darwin)
    (load-theme 'solarized-dark t))
   (t
    (load-theme 'solarized-dark t))))

(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

(use-package undo-tree
  :ensure t
  :diminish ""
  :config (global-undo-tree-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'reverse))

(use-package web-mode
  :ensure t)

(use-package windmove
  :config (windmove-default-keybindings))

(use-package yaml-mode
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(after-save-hook
   (quote
    (executable-make-buffer-file-executable-if-script-p)))
 '(backup-directory-alist (quote (("\\.*$" . "~/.emacs.d/backup"))))
 '(before-save-hook (quote (delete-trailing-whitespace)))
 '(column-number-mode t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(dired-dwim-target t)
 '(haskell-process-type (quote stack-ghci))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(kill-whole-line t)
 '(open-junk-file-find-file-function (quote find-file))
 '(require-final-newline (quote visit))
 '(ruby-insert-encoding-magic-comment nil)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(scroll-margin 5)
 '(scss-compile-at-save nil)
 '(show-trailing-whitespace t)
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
